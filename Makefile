CABAL ?= cabal
GEN_INIT_CPIO ?= gen_init_cpio
GREP ?= grep
GZIP ?= gzip
LDDTREE ?= lddtree
QEMU = qemu-system-x86_64

TEST_ON_KERNEL ?= $(shell $(CABAL) exec command -- -v test-on-kernel)

BZIMAGE ?= bzImage
INITRAMFS ?= initramfs

default: run

run: $(INITRAMFS) $(BZIMAGE)
	$(QEMU) \
		-nodefaults -no-user-config \
		-nographic \
		-no-reboot \
		-enable-kvm \
		-cpu host \
		-M microvm,x-option-roms=off,pit=off,pic=off,isa-serial=off,rtc=off \
		-no-acpi \
		-chardev stdio,id=virtiocon0 \
		-device virtio-serial-device \
		-device virtconsole,chardev=virtiocon0 \
		-kernel $(BZIMAGE) \
		-initrd $(INITRAMFS) \
		-append "console=hvc0 reboot=t quiet"
.PHONY: run

initramfs: initramfs.cpio
	$(GZIP) -c -9 < $< > $@

initramfs.cpio: initramfs.cpiolist $(TEST_ON_KERNEL)
	$(GEN_INIT_CPIO) -t 0 -c - < $< > $@

initramfs.cpiolist: test-on-kernel.lddtree
	echo "" > $@
	echo "dir /dev 0755 0 0" >> $@
	echo "nod /dev/null 0644 0 0 c 1 3" >> $@
	echo "nod /dev/random 0644 0 0 c 1 8" >> $@
	echo "nod /dev/kmsg 0644 0 0 c 1 11" >> $@
	echo "nod /dev/urandom 0644 0 0 c 1 9" >> $@
	echo "nod /dev/console 0644 0 0 c 5 1" >> $@
	echo "file /init $(TEST_ON_KERNEL) 0755 0 0" >> $@
	echo "dir /lib64 0755 0 0" >> $@
	cat $< | $(GREP) -v "$(TEST_ON_KERNEL)" | while read line; do echo "file $$line $$line 0755 0 0" >> $@; done

test-on-kernel.lddtree: $(TEST_ON_KERNEL)
	$(CABAL) exec $(LDDTREE) -- -l $(TEST_ON_KERNEL) > $@
