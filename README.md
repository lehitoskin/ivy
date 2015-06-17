Ivy, the Taggable Image Viewer
==============================

Ivy is an image viewer that allows the user to set or remove tags that are
tracked solely by the application. Ivy operates as follows:

- To load an image, call ivy and then the path to the image. Ivy can also handle
  multiple images and will put them into a collection, where only the images
  specified will be shown (until you specify to open a new image, of course).
  Example: `ivy "praise the sun.jpg"` or
  `ivy ../vacation/DMC/001.JPG /tmp/icon.png`

- To add a tag or list of tags to an image, simply enter them into the tag text
  field, separated by a comma and a space. E.g. `flower, beach, roller coaster`
  and then press the "Set" button or the Enter key.

- To remove any set tags, simply delete the contents in the tag text field,
  press the "Set" button or Enter key, and the entry for that image will be
  deleted.

Ivy cannot yet display animation in gifs due to the nature of the GUI library,
but for now a static image will be displayed instead.
