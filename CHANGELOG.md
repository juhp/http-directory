# Changelog

`http-directory` uses [PVP Versioning](https://pvp.haskell.org).

## 0.1.5 (2019-06-xx)
- also filter ".." and "#" from httpDirectory
- export Manager
- add isHttpUrl

## 0.1.4 (2019-06-07)
- add httpRawDirectory
- httpDirectory now filters out absolutes hrefs and sort links
- add httpDirectory' and httpRedirect' variants with own Manager

## 0.1.3 (2019-06-04)
- add httpExists

## 0.1.2 (2019-05-07)
- add httpManager convenient function
- print url when result not 200
- add an example

## 0.1.1 (2019-04-14)
- add httpLastModified

## 0.1.0 (2019-04-11)
- initial release with httpDirectory, httpFileSize, and httpRedirect(s)
