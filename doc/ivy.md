% IVY(1) Ivy Image Viewer | Version 2.3.1

# NAME
Ivy Image Viewer

# SYNOPSIS
ivy [option ... ] [args] ...

# DESCRIPTION

Calling Ivy without a path will simply open the GUI.
Supplying a path will tell Ivy to load the provided image.
Supplying multiple paths will tell Ivy to load them as a collection.

## GUI Interface
OS X note: All Ctrl- keyboard shortcuts are available as
Cmd-.

- Calling *ivy* by itself will start Ivy with the image-viewing area blank. You
  can then load images from the menu (File -> Open), or with the keyboard
  shortcut Ctrl-O.

- To load an image right away, call *ivy* and then the path to the image.
  For example: *ivy "praise the sun.jpg"*

- Once you have loaded an image, you can view all the other images in the same
  folder, cycling through the image files by either clicking on the arrow
  buttons or hitting the arrow keys. Pressing the Home or End keys will take you
  to the first and last images available respectively.

- If you want to view images from disparate locations, Ivy can also create
  a temporary collection, so that you only cycle through the set of images that
  you specified. For example, *ivy ~/Downloads/cat\*.jpg ~/Pictures/dog\*.jpg*
  will allow you to look at all your downloaded cat pictures and all the photos
  you've taken of your dog, and only those photos. (Assuming, of course, that
  your cat and dog pictures have "cat" and "dog" at the beginning of their
  filenames.) The same effect occurs when selecting multiple images from File ->
  Open.

- You may append files to a collection through File -> Append, or with the
  keyboard shortcut Ctrl-Shift-O. When using
  the Append functionality on an empty collection (e.g. when first opening Ivy)
  only the files selected will be available for viewing. Appending files is also
  accomplished by the drag-and-drop feature: simply drag and drop images onto
  the canvas and they will be appeneded to the collection.

- To add a tag or list of tags to an image, simply enter them into the tag text
  field, separated by a comma. E.g., *flower, beach, roller coaster* and then
  press the "Set" button or the Enter key.

- If you are currently editing tags and wish to revert any unsaved changes, simply
  hit the Esc key. Hitting Esc when there's no pending changes
  to be saved will unfocus the tag text field.

- To remove any set tags, simply delete the contents in the tag text field,
  press the "Set" button or Enter key, and the entry for that image will be
 
- To set a rating for an image, simply select a number (with a neat unicode
  shining star) from the choice selection drop-down menu. 0 (the default)
  means the picture hasn't yet been rated, -1 means the image should be rejected
  from the rating system, and numbers 1 through 5 mean how you rate this image
  against its peers (with 1 being the lowest and 5 being the highest). This
  rating is also available from the Metadata Editor under the tag "xmp:Rating".

- Sorting a collection may now be done from the View menu. Available sorting
  methods are by alphabetical name (the default), by the highest image rating,
  deleted.

## Tag Browser

Ivy comes with a tag browser mode (available from the `View` menu or with
<kbd>Ctrl</kbd>-<kbd>B</kbd>) so that you may view every tag category and its
contents. Clicking on a tag will show the images you have tagged. Clicking on
the image will show a thumbnail preview which is itself clickable. Clicking on
the thumbnail button will tell Ivy to load the source image for further viewing.

From the tag browser it is also possible to delete entire tag categories or
rename them. Also, it is possible to edit the taglist of an image by clicking on
an image path and selecting "Edit Tags" from the menu.

## Animated GIF Support

Animated GIF support is currently marked as experimental. To animate a GIF,
select View -> Animation. Due to the unstable nature of the related code, know
that some GIF's may not load properly or at all.

## FLIF Support

FLIF support is marked as experimental! To load a FLIF file in Ivy, simply open
like any other image. Currently, the FLIF decoder is not optimized, so decoding
takes much longer than for other formats.

## Embedding Tags as XMP metadata

Ivy embeds the taglist as XMP metadata in the file itself (if that file supports
XMP metadata). That way if you move your images around, the tags will stay the
same. However, the information in the database will then be out of date, so it
is recommended that if you move files around, utilize Ivy's command-line
interface with the switch `-M` to ensure the changes are tracked.

# OPTIONS

## Mutually exclusive options

### -V, --version
Display Ivy version.

### -o , --search-or *[taglist]*
Search the tags database inclusively with a comma-separated string.

### -a, --search-and *[taglist]*
Search the tags database exclusively with a comma-separated string.

### -L, --list-tags *[imagelist]*
Lists the tags for the image(s).

### -A, --add-tags *[taglist]* *[imagelist]*
Add tags to an image. ex: ivy -A "tag0, tag1, ..." /path/to/image ...

### -D, --delete-tags *[taglist]* *[imagelist]*
Delete tags from image. ex: ivy -D "tag0, tag1, ..." /path/to/image ...

### -P, --purge *[imagelist]*
Remove all tags from the images and purge from the database. ex: ivy -P /path/to/image ...

### -T, --set-tags *[taglist]* *[imagelist]*
Sets the taglist of the image. ex: ivy -T "tag0, tag1, ..." /path/to/image ...

### -M, --move-image *[image(s) ... destination]*
Moves the source file(s) to the destination, updating the database.

### ---show-xmp *[imagelist]*
Shows the XMP metadata for supported files.

### ---set-xmp *[xmp string]* *[imagelist]*
Sets the XMP metadata for supported files in imagelist.

### --show-rating *[imagelist]*
Show the xmp:Rating of the images as recorded in the database.

### --set-rating *[rating]* *[imagelist]*
Set the xmp:Rating for the images in both the database and the XMP.

## May be specified once each

### -e, --exact-search
Search the tags database for exact matches.

### -x, --exclude *[taglist]*
Search the tags database with -o/-a, but exclude images with the specified tags.

### -n, --null
Search result items are terminated by a null character instead of by whitespace.

### -v, --verbose
Display verbose information for certain operations.

### --help, -h
Show this help

### --
Do not treat any remaining argument as a switch (at this level)


Multiple single-letter switches can be combined after one *-*; for example: *-h-* is the same as *-h --*
