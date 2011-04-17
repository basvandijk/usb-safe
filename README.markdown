The [usb] package provides a standard Haskell abstraction layer over
[bindings-libusb] providing: abstract types instead of `Ptr`s, automatic
marshalling and unmarshalling, automatic garbage collection, exceptions instead
of integer return codes, etc..

While all that is very nice there are still some things that you can do
wrong. For example doing I/O with a closed device or reading from or writing to
an endpoint which doesn't belong to the claimed interface. Or reading from an
Out endpoint or writing to an In endpoint.

`usb-safe` provides the following guarantees:

* You can't reference handles to devices that are closed. In other words: no I/O
   with closed handles is possible.

* The programmer specifies the *region* in which devices should remain open. On
  exit from the region the opened devices will be closed automatically.

* You can't reference handles to configurations that have not been set.

* You can't reference handles to interfaces that have not been claimed.

* Just like with devices, the programmer can specify the region in which
  interfaces should remain claimed. On exit from the region the claimed
  interfaces will be released automatically.

* You can't reference handles to alternates that have not been set.

* You can't reference endpoints that don't belong to a setted alternate.

* You can't read from an endpoint with an Out transfer direction.

* You can't write to an endpoint with an In transfer direction.

* You can't read from or write to endpoints with the unsupported transfer types
  Control and Isochronous. Only I/O with endpoints with the Bulk and Interrupt
  transfer types is allowed.

The primary technique used in `usb-safe` is called "Lightweight monadic regions"
which was [invented][1] by Oleg Kiselyov and Chung-chieh Shan.

This technique is implemented in the [regions] package which is re-exported from
`usb-safe`.

See the [usb-safe-examples] package for examples how to use this library:

    git clone git://github.com/basvandijk/usb-safe-examples.git

[1]: http://okmij.org/ftp/Haskell/regions.html#light-weight

[bindings-libusb]: http://hackage.haskell.org/package/bindings-libusb
[usb]:             http://hackage.haskell.org/package/usb
[regions]:         http://hackage.haskell.org/package/regions

[usb-safe-examples]: https://github.com/basvandijk/usb-safe-examples
