
unsigned int TranslateAddr(unsigned int addr) {
	addr -= 0x20000000;
	addr += 0x3f000000;
	return addr;
}
