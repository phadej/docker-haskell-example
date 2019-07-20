---
title: Embedding secret data into Docker images
author: Oleg Grenrus
tags: engineering
---

In [Multi-stage docker build of Haskell webapp](2019-07-04-docker-haskell-example.html)
blog post I briefly mentioned `data-files`. They are problematic.
A simpler way is to use e.g. [`file-embed-lzma`](https://hackage.haskell.org/package/file-embed-lzma)
or similar functionality to *embed data* into the final binary.

You can also embed *secret data* if you first *encrypt* it.  This would reduce
the pain when dealing with (large) secrets.  I personally favor configuration
(of running Docker containers) through environment variables. Injecting extra
data into containers is inelegant: another way to "configure" running
container.

In this blog post, I'll show that dealing with encrypted data in Haskell
is not too complicated.
The code is in the [same repository](https://github.com/phadej/docker-haskell-example)
as the previous post.
This post is based on
[Tutorial: AES Encryption and Decryption with OpenSSL](https://eclipsesource.com/blogs/2017/01/17/tutorial-aes-encryption-and-decryption-with-openssl/),
but is updated and adapted for Haskell.

<div id="toc"></div>

Encrypting: OpenSSL Command Line
--------------------------------

To encrypt a plaintext using AES with OpenSSL, the `enc` command is used. The
following command will prompt you for a password, encrypt a file called
`plaintext.txt` and Base64 encode the output. The output will be written to
`encrypted.txt`.

```bash
openssl enc -aes-256-cbc -salt -pbkdf2 -iter 100000 -base64 -md sha1 -in plaintext.txt  -out encrypted.txt
```

This will result in a different output each time it is run. This is because a
different (random) salt is used. The *Salt* is written as part of the output,
and we will read it back in the next section.
I used `HaskellCurry` as a password, and placed an encrypted file in the repository.

Note that we use `-pbkdf2` flag. It's available since OpenSSL 1.1.1,
which *is* available in Ubuntu 18.04 at the time of writing.
Update your systems! We use 100000 iterations.

The choice of SHA1 digest is done because 
[`pkcs5_pbkdf2_hmac_sha1`](https://hackage.haskell.org/package/HsOpenSSL-0.11.4.16/docs/OpenSSL-EVP-Digest.html#v:pkcs5_pbkdf2_hmac_sha1)
exists directly in `HsOpenSSL`.
We will use it to derive key and IV from a password in Haskell.
Alternatively, you could use `-p` flag, so
`openssl` prints the used Key and IV and provide these to the running
service.

Decrypting: OpenSSL Command Line
--------------------------------

To decrypt file on command line, we'll use `-d` option:

```bash
openssl enc -aes-256-cbc -salt -pbkdf2 -iter 100000 -base64 -md sha1 -d -in encrypted.txt
```

This command is useful to check "what's there". Next, the Haskell version.

Decrypting: Haskell
-------------------

To decrypt the output of an AES encryption (aes-256-cbc) we will use the
[`HsOpenSSL`](https://hackage.haskell.org/package/HsOpenSSL) library.
Unlike the command line, each step must be explicitly performed.
Luckily, it's a lot nice that using C. There 6 steps:

1. Embed a file
2. Decode Base64
3. Extract the salt
4. Get a password
5. Compute the key and initialization vector
6. Decrypt the ciphertext

<h3>Embed a file</h3>

To embed file we use *Template Haskell*,
[`embedByteString`](https://hackage.haskell.org/package/file-embed-lzma-0/docs/FileEmbedLzma.html#v:embedByteString)
from `file-embed-lzma` library.

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Data.ByteString (ByteString)
import FileEmbedLzma (embedByteString)

encrypted :: ByteString
encrypted = $(embedByteString "encrypted.txt")
```

<h3>Decode Base64</h3>

Decoding Base64 is an one-liner in Haskell.
We use [`decodeLenient`](https://hackage.haskell.org/package/base64-bytestring-1.0.0.2/docs/Data-ByteString-Base64.html#v:decodeLenient)
because we are quite sure input is valid.

```haskell
import Data.ByteString.Base64 (decodeLenient)

encrypted' :: ByteString
encrypted' = decodeLenient encrypted
```

Note: `HsOpenSSL` can also handle Base64, but doesn't seem to provide
lenient variant. `HsOpenSSL` throws exceptions on errors.

<h3>Extract the salt</h3>

Once we have decoded the cipher, we can read the salt.
The Salt is identified by the 8 byte header (`Salted__`),
followed by the 8 byte salt.
We start by ensuring the header exists, and then we extract the following 8 bytes:

```haskell
extract
    :: ByteString     -- ^ password
    -> ByteString     -- ^ encrypted data
    -> IO ByteString  -- ^ decrypted data
extract password bs0 = do
    when (BS.length bs0 < 16) $ fail "Too small input"

    let (magic, bs1) = BS.splitAt 8 bs0
        (salt,  enc) = BS.splitAt 8 bs1

    when (magic /= "Salted__") $ fail "No Salted__ header"

    ...
```

<h3>Get a password</h3>

We use `unix` package,
and [`System.Posix.Env.ByteString.getEnv`](https://hackage.haskell.org/package/unix-2.7.2.2/docs/System-Posix-Env-ByteString.html#v:getEnv)
to get environment variable as `ByteString` directly.
The program will run in Docker in Linux: depending on `unix` is not a problem.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import System.Posix.Env.ByteString (getEnv)
import OpenSSL (withOpenSSL)

main :: IO ()
main = withOpenSSL $ do
    password <- getEnv "PASSWORD" >>= maybe (fail "PASSWORD not set") return
    ... 
```

We also initialize the OpenSSL library using [`withOpenSSL`](https://hackage.haskell.org/package/HsOpenSSL-0.11.4.16/docs/OpenSSL.html#v:withOpenSSL).

<h3>Compute the key and initialization vector</h3>

Once we have extracted the salt, we can use the salt and password to generate
the Key and Initialization Vector (IV). To determine the Key and IV from the
password we use the
[`pkcs5_pbkdf2_hmac_sha1`](https://hackage.haskell.org/package/HsOpenSSL-0.11.4.16/docs/OpenSSL-EVP-Digest.html#v:pkcs5_pbkdf2_hmac_sha1)
function. PBKDF2 (Password-Based Key Derivation Function 2) is
a key derivation function. We (as `openssl`) derive both key and IV simultaneously:

```haskell
import OpenSSL.EVP.Digest (pkcs5_pbkdf2_hmac_sha1)

iters :: Int
iters = 100000

    ...
    let (key, iv) = BS.splitAt 32
                  $ pkcs5_pbkdf2_hmac_sha1 password salt iters 48
    ...
```

<h3>Decrypting the ciphertext</h3>

With the Key and IV computed, and the ciphertext decoded from Base64, we are now
ready to decrypt the message.

```haskell
import OpenSSL.EVP.Cipher (getCipherByName, CryptoMode(Decrypt), cipherBS)

    ...
    cipher <- getCipherByName "aes-256-cbc"
              >>= maybe (fail "no cipher") return
    plain <- cipherBS cipher key iv Decrypt enc
    ...
```

Conclusion
----------

In this post we embedded an encrypted file into Haskell application,
which is then decrypted at run time. The complete copy of the code is
at [same repository](https://github.com/phadej/docker-haskell-example),
and changes done for this post are visible in
[a pull request](https://github.com/phadej/docker-haskell-example/pull/1).
