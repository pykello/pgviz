#include "common.h"


#define UART0_BASE   0x20201000
#define UART0_DR     (UART0_BASE+0x00)
#define UART0_RSRECR (UART0_BASE+0x04)
#define UART0_FR     (UART0_BASE+0x18)
#define UART0_ILPR   (UART0_BASE+0x20)
#define UART0_IBRD   (UART0_BASE+0x24)
#define UART0_FBRD   (UART0_BASE+0x28)
#define UART0_LCRH   (UART0_BASE+0x2C)
#define UART0_CR     (UART0_BASE+0x30)
#define UART0_IFLS   (UART0_BASE+0x34)
#define UART0_IMSC   (UART0_BASE+0x38)
#define UART0_RIS    (UART0_BASE+0x3C)
#define UART0_MIS    (UART0_BASE+0x40)
#define UART0_ICR    (UART0_BASE+0x44)
#define UART0_DMACR  (UART0_BASE+0x48)
#define UART0_ITCR   (UART0_BASE+0x80)
#define UART0_ITIP   (UART0_BASE+0x84)
#define UART0_ITOP   (UART0_BASE+0x88)
#define UART0_TDR    (UART0_BASE+0x8C)

uint32_t GET32(uint32_t addr) {
	return (*(volatile uint32_t*) TranslateAddr(addr));
}
void PUT32(uint32_t addr, uint32_t val) {
	(*(volatile uint32_t*) TranslateAddr(addr)) = val;
}

uint16_t GET16(uint32_t addr) {
	return (*(volatile uint16_t*) TranslateAddr(addr));
}
void PUT16(uint32_t addr, uint16_t val) {
	(*(volatile uint16_t*) TranslateAddr(addr)) = val;
}


void uart0_init(void) {
	int ra;
	PUT32(GPFSEL1, GET32(GPFSEL1) &
				   ~((uint32_t)0x07 << ((14 % 10) * 3)) &
				   ~((uint32_t)0x07 << ((15 % 10) * 3)));
	PUT32(GPFSEL1, GET32(GPFSEL1) |
				   ((uint32_t)0x04 << ((14 % 10) * 3)) |
				   ((uint32_t)0x04 << ((15 % 10) * 3)));
	PUT32(GPPUD,0);
    for(ra=0;ra<150;ra++);
    PUT32(GPPUDCLK0,(1<<14)|(1<<15));
    for(ra=0;ra<150;ra++);
    PUT32(GPPUDCLK0,0);
}

void uart0_putc(int c) {
	if (c == '\n') uart0_putc('\r');
	while(1)
    {
        if((GET32(UART0_FR)&0x20)==0) break;
    }
    PUT32(UART0_DR,c);
}

int uart0_getc(void) {
	return GET32(UART0_DR);
}

void uart0_print(const char *s) {
	while (*s) {
		uart0_putc(*s);
		for(int j=0; j<10000; j++);
		s++;
	}
}

void print_binary(uint32_t n) {
	for (int i = 31; i >= 0; i--) {
		uart0_putc('0' + ((n & (1u << i)) ? 1 : 0));
	}
	uart0_putc('\r');
	uart0_putc('\n');
}

