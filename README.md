Ivy, the Taggable Image Viewer
==============================

Ivy is an image viewer that allows the user to set or remove tags that are
tracked solely by the application.


## Installation

Ivy requires [Racket](http://racket-lang.org/) version 6.0 and up. Beyond that,
you actually don't *need* to install Ivy, per se. Simply point the Racket
interpreter to the location of the `main.rkt` file. However, it will be easier
to use Ivy if the executable is in your PATH. Compiling Ivy is simple. In the
directory where you saved Ivy, you can just run

```
make
sudo make install
```

You may change the DESTDIR variable to install ivy to an arbitrary location -
one that may not require super user privileges.

Or on Windows:

``` bash
raco exe -o ivy main.rkt
```

This creates an executable binary (called `ivy`), which you can then move to
any directory in your PATH. You should then be able to call `ivy` from
anywhere.


## Usage

- Calling `ivy` by itself will start Ivy with the image-viewing area blank. You
  can then load images from the menu (File -> Open), or with the keyboard
  shortcut <kbd>Ctrl</kbd>-<kbd>O</kbd>.

- To load an image right away, call `ivy` and then the path to the image.
  For example: `ivy "praise the sun.jpg"`

- Once you have loaded an image, you can view all the other images in the same
  folder, cycling through the image files by either clicking on the arrow
  buttons or hitting the arrow keys. Pressing the home or end keys will take you
  to the first and last images available respectively.

- If you want to view images from disparate locations, Ivy can also create
  a temporary collection, so that you only cycle through the set of images that
  you specified. For example, `ivy ~/Downloads/cat*.jpg ~/Pictures/dog*.jpg`
  will allow you to look at all your downloaded cat pictures and all the photos
  you've taken of your dog, and only those photos. (Assuming, of course, that
  your cat and dog pictures have "cat" and "dog" at the beginning of their
  filenames.) The same effect occurs when selecting multiple images from File ->
  Open.

- You may append files to a collection through File -> Append, or with the
  keyboard shortcut <kbd>Ctrl</kbd>-<kbd>Shift</kbd>-<kbd>O</kbd>. When using
  the Append functionality on an empty collection (e.g. when first opening Ivy)
  only the files selected will be available for viewing.

- To add a tag or list of tags to an image, simply enter them into the tag text
  field, separated by a comma and a space. E.g., `flower, beach, roller
  coaster` and then press the "Set" button or the Enter key.

- To remove any set tags, simply delete the contents in the tag text field,
  press the "Set" button or Enter key, and the entry for that image will be
  deleted.

Ivy cannot yet display animation in gifs due to a limitation of the GUI
library. A static image will be displayed instead.
