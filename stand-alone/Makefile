DESTDIR=/
ICONSIZES=16 24 32 48 64 128

.PHONY: install
install:
	install -Dm755 helm-system-packages \
		"$(DESTDIR)/usr/bin/helm-system-packages"
	install -Dm644 helm-system-packages.desktop \
		"$(DESTDIR)/usr/share/applications/helm-system-packages.desktop"
	$(foreach i,$(ICONSIZES),install -Dm644 "helm-$(i)x$(i).png" \
			"$(DESTDIR)/usr/share/icons/hicolor/$(i)x$(i)/apps/helm.png";)
