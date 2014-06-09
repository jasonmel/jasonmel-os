ASM = nasm
CC  = gcc -Wall
LD  = ld
FD_IMG = fd.img

fd.img: bootsect.img ftable.img kernel.img
	dd if=/dev/zero of=fd.img bs=1440k count=1
	dd if=bootsect.img of=$(FD_IMG) conv=sync,notrunc
	dd if=ftable.img of=$(FD_IMG) conv=sync,notrunc seek=1
	dd if=kernel.img of=$(FD_IMG) conv=sync,notrunc seek=2
#	dd if=hello1 of=$(FD_IMG) conv=sync,notrunc seek=7
#	dd if=hello2 of=$(FD_IMG) conv=sync,notrunc seek=12
	dd if=kernel.img of=$(FD_IMG) conv=sync,notrunc seek=51

bootsect.img: bootsect.asm
	$(ASM) -o bootsect.img bootsect.asm

ftable.img: ftable.asm
	$(ASM) -o ftable.img ftable.asm

kernel.img: kernel.asm
	$(ASM) -o kernel.img kernel.asm

clean:
	rm -rf *.img

