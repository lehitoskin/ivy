Ivy, the Taggable Image Viewer
==============================

Ivy is an image viewer that allows the user to set or remove tags that are
tracked solely by the application.

## Installation

### Dependencies

- [Racket](http://racket-lang.org/) version 6.6 and up
- [gif-image](https://github.com/lehitoskin/gif-image)
- [png-image](https://github.com/lehitoskin/png-image)
- [racquel](https://github.com/brown131/racquel)
- [riff](https://github.com/lehitoskin/riff) (which uses FLIF)
- [rsvg](https://github.com/takikawa/rsvg) (which uses librsvg)
- [sugar](https://github.com/mbutterick/sugar)
- [txexpr](https://github.com/mbutterick/txexpr)

### Compilation

You actually don't *need* to install Ivy, per se. Simply point the Racket
interpreter to the location of the `main.rkt` file. However, it will be
easier to use Ivy if the executable is in your PATH. Compiling Ivy is simple:
in the directory where you saved Ivy, you can just run

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

OS X note: `make` will generate an .app bundle, and `make install` by default will
place it in `/Applications/`.


## Usage

### GUI Interface

OS X note: All <kbd>Ctrl</kbd>- keyboard shortcuts are available as
<kbd>Cmd</kbd>-.

- Calling `ivy` by itself will start Ivy with the image-viewing area blank. You
  can then load images from the menu (File -> Open), or with the keyboard
  shortcut <kbd>Ctrl</kbd>-<kbd>O</kbd>.

- To load an image right away, call `ivy` and then the path to the image.
  For example: `ivy "praise the sun.jpg"`

- Once you have loaded an image, you can view all the other images in the same
  folder, cycling through the image files by either clicking on the arrow
  buttons or hitting the arrow keys. Pressing the Home or End keys will take you
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
  only the files selected will be available for viewing. Appending files is also
  accomplished by the drag-and-drop feature: simply drag and drop images onto
  the canvas and they will be appeneded to the collection.

- To add a tag or list of tags to an image, simply enter them into the tag text
  field, separated by a comma. E.g., `flower, beach, roller coaster` and then
  press the "Set" button or the <kbd>Enter</kbd> key.

- If you are currently editing tags and wish to revert any unsaved changes, simply
  hit the <kbd>Esc</kbd> key. Hitting <kbd>Esc</kbd> when there's no pending changes
  to be saved will unfocus the tag text field.

- To remove any set tags, simply delete the contents in the tag text field,
  press the "Set" button or <kbd>Enter</kbd> key, and the entry for that image will be
  deleted.

- To set a rating for an image, simply select a number (with a neat unicode
  shining star) from the choice selection drop-down menu. 0 (the default)
  means the picture hasn't yet been rated, -1 means the image should be rejected
  from the rating system, and numbers 1 through 5 mean how you rate this image
  against its peers (with 1 being the lowest and 5 being the highest). This
  rating is also available from the Metadata Editor under the tag "xmp:Rating".

- Sorting a collection may now be done from the View menu. Available sorting
  methods are by alphabetical name (the default), by the highest image rating,
  and by the lowest image rating.

### Tag Browser

Ivy comes with a tag browser mode (available from the `View` menu or with
<kbd>Ctrl</kbd>-<kbd>B</kbd>) so that you may view every tag category and its
contents. Clicking on a tag will show the images you have tagged. Clicking on
the image will show a thumbnail preview which is itself clickable. Clicking on
the thumbnail button will tell Ivy to load the source image for further viewing.

From the tag browser it is also possible to delete entire tag categories or
rename them. Also, it is possible to edit the taglist of an image by clicking on
an image path and selecting "Edit Tags" from the menu.

### Animated GIF Support

Animated GIF support is currently marked as experimental. To animate a GIF,
select View -> GIF Animation. Due to the unstable nature of the related code,
know that some GIF's may not load properly or at all.

### FLIF Support

[FLIF](https://github.com/FLIF-hub/FLIF) support is marked as experimental! To
load a FLIF file in Ivy, simply open like any other image. Ivy will load the
image progressively and a little percentage meter will appear prepended to
the image's filename in the title of the frame.

### Embedding Tags as XMP metadata

Ivy embeds the taglist as XMP metadata in the file itself (if that file supports
XMP metadata). That way if you move your images around, the tags will stay the
same. However, the information in the database will then be out of date, so it
is recommended that if you move files around, utilize Ivy's command-line
interface with the switch `-M` to ensure the changes are tracked.
