# tools
AR = arm-none-eabi-ar
AS = arm-none-eabi-as
CC = arm-none-eabi-gcc
LD = arm-none-eabi-ld
OBJCOPY = arm-none-eabi-objcopy
OBJDUMP = arm-none-eabi-objdump

# flags
CPU = arm926ej-s
CFLAGS = -mcpu=$(CPU) -gstabs -marm \
         -std=c99 -pedantic -Wall -Wextra -msoft-float -fPIC -mapcs-frame \
         -fno-builtin-printf -fno-builtin-strcpy -Wno-overlength-strings \
         -fno-builtin-exit -I.
ASFLAGS = -mcpu=$(CPU) -g

OBJS = startup.o src/main.o src/uart0.o \
	   src/sd.o src/printf.o src/mailbox.o src/wdog.o

raspi-kernel.img: $(OBJS) src/raspi.o linker.ld
	arm-none-eabi-ld -Ttext 0x8000 -T linker.ld $(OBJS) src/raspi.o -o raspi-kernel.elf
	arm-none-eabi-objcopy -O binary raspi-kernel.elf raspi-kernel.img

raspi2-qemu.img: $(OBJS) src/raspi2.o linker.ld
	arm-none-eabi-ld -Ttext 0x10000 -T linker.ld $(OBJS) src/raspi2.o -o raspi2-qemu.elf
	arm-none-eabi-objcopy -O binary raspi2-qemu.elf raspi2-qemu.img

qemu: raspi2-qemu.img
	qemu-system-arm -M raspi2 -m 512M -nographic -sd tests/sdcard.img -kernel raspi2-qemu.img

test-qemu: raspi2-qemu.img
	racket tests/test-qemu.rkt

test-raspi: raspi-kernel.img
	time sudo racket tests/test-raspi.rkt

clean:
	rm -f $(OBJS) raspi*.o *.elf *.bin raspi*.img
