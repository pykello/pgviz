#include "common.h"

#define MBOX_BASE 0x2000b880

#define MBOX_PEEK 0x10
#define MBOX_READ 0x00
#define MBOX_WRITE 0x20
#define MBOX_STATUS 0x18
#define MBOX_SENDER 0x14
#define MBOX_CONFIG 0x1c

#define MBOX_FB		1
#define MBOX_PROP	8

#define MBOX_SUCCESS	0x80000000

#define MBOX_FULL		0x80000000
#define	MBOX_EMPTY		0x40000000

uint32_t mailbox_read(uint8_t channel)
{
	while(1)
	{
		while(GET32(MBOX_BASE + MBOX_STATUS) & MBOX_EMPTY);

		uint32_t data = GET32(MBOX_BASE + MBOX_READ);
		uint8_t read_channel = (uint8_t)(data & 0xf);
		if(read_channel == channel)
			return (data & 0xfffffff0);
	}
}


void mailbox_write(uint8_t channel, uint32_t data)
{
	while(GET32(MBOX_BASE + MBOX_STATUS) & MBOX_FULL);
	PUT32(MBOX_BASE + MBOX_WRITE, (data & 0xfffffff0) | (uint32_t)(channel & 0xf));
}


__attribute__((__aligned__(0x10)))
volatile uint32_t mailbuffer[20];

uint32_t sd_get_base_clock_hz(void)
{
    uint32_t base_clock;

	/* Get the base clock rate */
	// set up the buffer
	mailbuffer[0] = 8 * 4;		// size of this message
	mailbuffer[1] = 0;			// this is a request

	// next comes the first tag
	mailbuffer[2] = 0x00030002;	// get clock rate tag
	mailbuffer[3] = 0x8;		// value buffer size
	mailbuffer[4] = 0x4;		// is a request, value length = 4
	mailbuffer[5] = 0x1;		// clock id + space to return clock id
	mailbuffer[6] = 0;			// space to return rate (in Hz)

	// closing tag
	mailbuffer[7] = 0;

	// send the message
	mailbox_write(MBOX_PROP, (uint32_t) mailbuffer);

	// read the response
	mailbox_read(MBOX_PROP);

	if(mailbuffer[1] != MBOX_SUCCESS)
	{
	    uart0_printf("EMMC: property mailbox did not return a valid response.\n");
	    return 0;
	}

	if(mailbuffer[5] != 0x1)
	{
	    uart0_printf("EMMC: property mailbox did not return a valid clock id.\n");
	    return 0;
	}

	base_clock = mailbuffer[6];

#ifdef EMMC_DEBUG
    uart0_printf("EMMC: base clock rate is %d Hz\n", base_clock);
#endif
    return base_clock;
}

static int bcm_2708_power_off()
{
	/* Power off the SD card */
	// set up the buffer
	mailbuffer[0] = 8 * 4;		// size of this message
	mailbuffer[1] = 0;			// this is a request

	// next comes the first tag
	mailbuffer[2] = 0x00028001;	// set power state tag
	mailbuffer[3] = 0x8;		// value buffer size
	mailbuffer[4] = 0x8;		// is a request, value length = 8
	mailbuffer[5] = 0x0;		// device id and device id also returned here
	mailbuffer[6] = 0x2;		// set power off, wait for stable and returns state

	// closing tag
	mailbuffer[7] = 0;

	// send the message
	mailbox_write(MBOX_PROP, (uint32_t) mailbuffer);

	// read the response
	mailbox_read(MBOX_PROP);

	if(mailbuffer[1] != MBOX_SUCCESS)
	{
	    uart0_printf("EMMC: bcm_2708_power_off(): property mailbox did not return a valid response.\n");
	    return -1;
	}

	if(mailbuffer[5] != 0x0)
	{
	    uart0_printf("EMMC: property mailbox did not return a valid device id.\n");
	    return -1;
	}

	if((mailbuffer[6] & 0x3) != 0)
	{
#ifdef EMMC_DEBUG
		uart0_printf("EMMC: bcm_2708_power_off(): device did not power off successfully (%08x).\n", mailbuffer[6]);
#endif
		return 1;
	}

	return 0;
}

static int bcm_2708_power_on()
{
	/* Power on the SD card */
	// set up the buffer
	mailbuffer[0] = 8 * 4;		// size of this message
	mailbuffer[1] = 0;			// this is a request

	// next comes the first tag
	mailbuffer[2] = 0x00028001;	// set power state tag
	mailbuffer[3] = 0x8;		// value buffer size
	mailbuffer[4] = 0x8;		// is a request, value length = 8
	mailbuffer[5] = 0x0;		// device id and device id also returned here
	mailbuffer[6] = 0x3;		// set power off, wait for stable and returns state

	// closing tag
	mailbuffer[7] = 0;

	// send the message
	mailbox_write(MBOX_PROP, (uint32_t) mailbuffer);

	// read the response
	mailbox_read(MBOX_PROP);

	if(mailbuffer[1] != MBOX_SUCCESS)
	{
	    uart0_printf("EMMC: bcm_2708_power_on(): property mailbox did not return a valid response.\n");
	    return -1;
	}

	if(mailbuffer[5] != 0x0)
	{
	    uart0_printf("EMMC: property mailbox did not return a valid device id.\n");
	    return -1;
	}

	if((mailbuffer[6] & 0x3) != 1)
	{
#ifdef EMMC_DEBUG
		uart0_printf("EMMC: bcm_2708_power_on(): device did not power on successfully (%08x).\n", mailbuffer[6]);
#endif
		return 1;
	}

	return 0;
}

int bcm_2708_power_cycle(void)
{
	if(bcm_2708_power_off() < 0)
		return -1;

	for (int i = 0; i < 500000; i++);

	return bcm_2708_power_on();
}

