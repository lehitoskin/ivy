Ivy, the Taggable Image Viewer
==============================

Ivy is an image viewer that allows the user to set or remove tags that are
tracked solely by the application. Ivy operates as follows:

- Calling `ivy` by itself will start Ivy with the image-viewing area blank. You can
  then load images from the menu (File -> Open), or with the keyboard shortcut
  <kbd>Ctrl</kbd>-<kbd>O</kbd>.

- To load an image right away, call `ivy` and then the path to the image.
  Example: `ivy "praise the sun.jpg"`

- Once you have loaded an image, you can view all the other images in the same
  folder, cycling through the image files by either clicking on the arrow buttons
  or hitting the arrow keys.

- If you want to view images from disparate locations, Ivy can also create
  a temporary collection, so that you only cycle through the set of images
  that you specified. For example: `ivy ../vacation/DMC/001.JPG /tmp/icon.png`.
  The same effect occurs when selecting images from File -> Open.

- To add a tag or list of tags to an image, simply enter them into the tag text
  field, separated by a comma and a space. E.g. `flower, beach, roller coaster`
  and then press the "Set" button or the Enter key.

- To remove any set tags, simply delete the contents in the tag text field,
  press the "Set" button or Enter key, and the entry for that image will be
  deleted.

Ivy cannot yet display animation in gifs due to a limitation of the GUI library.
A static image will be displayed instead.
