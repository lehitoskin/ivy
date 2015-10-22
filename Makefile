DESTDIR=/usr/local

all: ivy

ivy: main.rkt base.rkt frame.rkt search-results.rkt
	raco exe --vv --gui -o ivy main.rkt

install: ivy ivy-image-viewer.desktop img/
	mkdir -pv $(DESTDIR)/bin
	mkdir -pv $(DESTDIR)/share/icons/hicolor/{16x16,32x32,48x48,64x64,128x128,256x256,512x512,scalable}/apps
	mkdir -pv $(DESTDIR)/share/applications
	sed "s:# Icon=/PATH/TO/ICON.png:Icon=$(DESTDIR)/share/icons/hicolor/128x128/apps/ivy-logo-128px.png:" ivy-image-viewer.desktop >ivy-image-viewer.desktop2
	sed -i "s:# Exec=/PATH/TO/EXECUTABLE:Exec=$(DESTDIR)/bin/ivy:" ivy-image-viewer.desktop2
	install -m 0755 ivy $(DESTDIR)/bin
	install -m 0644 img/ivy-logo-16px.png $(DESTDIR)/share/icons/hicolor/16x16/apps
	install -m 0644 img/ivy-logo-32px.png $(DESTDIR)/share/icons/hicolor/32x32/apps
	install -m 0644 img/ivy-logo-48px.png $(DESTDIR)/share/icons/hicolor/48x48/apps
	install -m 0644 img/ivy-logo-64px.png $(DESTDIR)/share/icons/hicolor/64x64/apps
	install -m 0644 img/ivy-logo-128px.png $(DESTDIR)/share/icons/hicolor/128x128/apps
	install -m 0644 img/ivy-logo-256px.png $(DESTDIR)/share/icons/hicolor/256x256/apps
	install -m 0644 img/ivy-logo-512px.png $(DESTDIR)/share/icons/hicolor/512x512/apps
	install -m 0644 img/ivy-logo.svg $(DESTDIR)/share/icons/hicolor/scalable/apps
	install -m 0755 ivy-image-viewer.desktop2 $(DESTDIR)/share/applications/ivy-image-viewer.desktop

clean:
	rm -Rv compiled/
	rm -v ivy-image-viewer.desktop2
