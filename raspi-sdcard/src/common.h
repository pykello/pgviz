#ifndef COMMON_H_
#define COMMON_H_

/* boolean */
typedef int bool;

/* integer types */
typedef __signed char int8_t;
typedef unsigned char uint8_t;
typedef short int16_t;
typedef unsigned short uint16_t;
typedef int int32_t;
typedef unsigned int uint32_t;
typedef uint32_t uintptr_t;
typedef uint32_t size_t;
typedef uint32_t useconds_t;

#define NULL 0

typedef __builtin_va_list va_list;

#define va_start(ap, last) __builtin_va_start(ap, last)
#define va_arg(ap, type) __builtin_va_arg(ap, type)
#define va_end(ap) __builtin_va_end(ap)

#define GPFSEL1    0x20200004
#define GPFSEL2    0x20200008
#define GPFSEL3    0x2020000c
#define GPFSEL4    0x20200010
#define GPFSEL5    0x20200014
#define GPSET0     0x2020001C
#define GPCLR0     0x20200028
#define GPPUD      0x20200094
#define GPPUDCLK0  0x20200098
#define GPLEV0     0x20200034
#define GPLEV1     0x20200038

#define MBOX_PROP	8
#define MBOX_SUCCESS	0x80000000


struct block_device {
	char *driver_name;
	char *device_name;
	uint8_t device_id[128];
	size_t dev_id_len;

	int supports_multiple_block_read;
	int supports_multiple_block_write;

	int (*read)(struct block_device *dev, uint8_t *buf, size_t buf_size, uint32_t block_num);
	int (*write)(struct block_device *dev, uint8_t *buf, size_t buf_size, uint32_t block_num);
	size_t block_size;
	size_t num_blocks;
};

struct sd_scr
{
    uint32_t    scr[2];
    uint32_t    sd_bus_widths;
    int         sd_version;
};

struct emmc_block_dev
{
	struct block_device bd;
	uint32_t card_supports_sdhc;
	uint32_t card_supports_18v;
	uint32_t card_ocr;
	uint32_t card_rca;
	uint32_t last_interrupt;
	uint32_t last_error;

	struct sd_scr scr;

	int failed_voltage_switch;

    uint32_t last_cmd_reg;
    uint32_t last_cmd;
	uint32_t last_cmd_success;
	uint32_t last_r0;
	uint32_t last_r1;
	uint32_t last_r2;
	uint32_t last_r3;

	void *buf;
	int blocks_to_transfer;
	size_t block_size;
	int use_sdma;
	int card_removal;
	uint32_t base_clock;
};


/* address stuff */
extern unsigned int TranslateAddr(unsigned int addr);
extern uint32_t GET32(uint32_t addr);
extern void PUT32(uint32_t addr, uint32_t val);
extern uint16_t GET16(uint32_t addr);
extern void PUT16(uint32_t addr, uint16_t val);

/* uart0 */
extern void uart0_init(void);
extern void uart0_putc(int c);
extern int uart0_getc(void);
extern void uart0_print(const char *s);
extern void print_binary(uint32_t n);

/* printf */
extern int vsprintf(char *target, const char *format, va_list ap);
extern int sprintf(char *target, const char *format, ...);
extern int uart0_printf(const char *format, ...);

/* sd */
extern int sd_card_init(struct block_device **dev);
extern int sd_read(struct block_device *dev, uint8_t *buf, size_t buf_size, uint32_t block_no);

/* mailbox */
extern uint32_t mailbox_read(uint8_t channel);
extern void mailbox_write(uint8_t channel, uint32_t data);
extern int bcm_2708_power_cycle(void);
extern uint32_t sd_get_base_clock_hz(void);


extern uint32_t unsigned_divmod(uint32_t numerator, uint32_t denominator,
			 uint32_t* remainder_pointer);
extern void *memcpy(void *target, const void *source, size_t n);

#endif
