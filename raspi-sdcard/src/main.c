
#include "common.h"


uint8_t buf[512];

static void reboot(void);

void c_entry(void)
{
	struct emmc_block_dev dev;
	struct block_device *devptr = (struct block_device *) &dev;
	uart0_init();

	sd_card_init(&devptr);
	uart0_printf("read %d bytes\n", sd_read(devptr, buf, 512, 0));
	uart0_printf("BYTES: ");
	for (int i = 0; i < 512; i++)
		uart0_printf("%x ", buf[i]);
	uart0_printf("\n");

	uart0_print("Bye!\r\n");

	reboot();
}


static void reboot(void)
{
	const int PM_RSTC = 0x2010001c;
	const int PM_WDOG = 0x20100024;
	const int PM_PASSWORD = 0x5a000000;
	const int PM_RSTC_WRCFG_FULL_RESET = 0x00000020;
	
	PUT32(PM_WDOG, PM_PASSWORD | 1); // timeout = 1/16th of a second? (whatever)
	PUT32(PM_RSTC, PM_PASSWORD | PM_RSTC_WRCFG_FULL_RESET);
	while(1);
}
