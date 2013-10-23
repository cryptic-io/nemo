Nemo
======

NOTE: This project has been abandoned in favor of [marlin][github.com/cryptic-io/marlin] combined with [generations][blog.mediocregopher.com/generations].

What is it?
-----------
A file server which will (at some point) automatically load balance and replicate itself.

How to use
----------
Nemo has two ports that it listens on: 80 (81 in testing, since nemo and the api are on the same server) and 9000. 9000 will be firewalled off from the public and is used for doing privileged commands (adding file keys, etc....), 80 is for public use.

To connect:
```nc <nemo ip> 80```

To use:
```{"command":"<command>","param1":"value1","param2":"value2","etc"..."}```

All nemo commands take the form of a json object with ```command``` being required.

Commands
--------
Current commands supported on the public interface:

*downloadFile*
```{"command":"downloadFile","filename":"<filename>","key":"<key for file>"}```

(The key to download the file must be obtained from the api).

When run this command returns the file's contents directly.

Http
----
Nemo ignores all lines sent to it that don't begin with a ```{```. This means http headers can be
safely sent with the actual json command code in the data section of the request. However, if you
want to receive http headers back (in the case of a browser, for instance) you need to specifically
tell nemo to do so. To do this:

```{"command":"<command>","param1":"value1","meta":{"http":true}}```

The headers sent back are pretty generic. In most cases Content-Type will be application/json, except
for any commands which return actualy files which will have a Content-Type of application/octet-stream.
