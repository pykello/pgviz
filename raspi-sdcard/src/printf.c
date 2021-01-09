#include "common.h"

/* digits */
#define DIGITS "0123456789abcdef"

/* forward declarations for local functions */
static int print_string(char *target, const char *str);
static int print_int(char *target, int number);
static int print_uint_in_base(char *target, uint32_t number, uint32_t base);


/*
 * uart0_printf formats and prints the given data. See vsprintf() for the format
 * flags currently supported.
 */
int
uart0_printf(const char *format, ...)
{
	int length = 0;
	char buffer[1024];
	int i = 0;
	va_list ap;

	va_start(ap, format);
	length = vsprintf(buffer, format, ap);
	va_end(ap);

	while (buffer[i] != '\0') {
		uart0_putc(buffer[i]);
		i++;
	}

	return length;
}

/*
 * sprintf formats the given data and outputs the result into the given character
 * pointer. See vsprintf for the format flags currently supported.
 */
int
sprintf(char *target, const char *format, ...)
{
	int length = 0;
	va_list ap;

	va_start(ap, format);
	length = vsprintf(target, format, ap);
	va_end(ap);

	return length;
}

/*
 * vsprintf formats the given data and returns the result in the given target
 * character pointer. Following format flags are currently supported:
 * 
 *   - %s: strings,
 *   - %c: characters,
 *   - %d: signed integers,
 *   - %u: unsigned integers,
 *   - %x: hexadecimal representation of integers.
 */
int vsprintf(char *target, const char *format, va_list ap)
{
	int format_index = 0;
	int target_index = 0;

	while (format[format_index] != 0) {
		char format_flag = 0;

		while (format[format_index] != '%' &&
		       format[format_index] != '\0')
		{
			target[target_index] = format[format_index];
			target_index++;
			format_index++;
		}

		if (format[format_index] == 0 || format[format_index + 1] == 0)
			break;

		format_flag = format[format_index + 1];
		switch (format_flag) {
		/* string */
		case 's':
		{
			const char *string_arg = va_arg(ap, char *);
			target_index += print_string(target + target_index,
						     string_arg);

			break;
		}
		/* char */
		case 'c':
		{
			target[target_index] = (char) va_arg(ap, int);
			target_index++;

			break;
		}
		/* int */
		case 'd':
		{
			int int_arg = va_arg(ap, int);
			target_index += print_int(target + target_index,
						  int_arg);

			break;
		}
		/* unsigned int */
		case 'u':
		{
			uint32_t uint_arg = va_arg(ap, uint32_t);
			target_index += print_uint_in_base(target + target_index,
							   uint_arg, 10);

			break;
		}
		/* hexadecimal */
		case 'x':
		{
			int uint_arg = va_arg(ap, uint32_t);
			target_index += print_string(target + target_index, "0x");
			target_index += print_uint_in_base(target + target_index,
							   uint_arg, 16);

			break;
		}
		}

		/* skip % and format_flag */
		format_index += 2;
	}

	target[target_index] = 0;

	return target_index;
}

static int print_string(char *target, const char *str)
{
	int target_index = 0;
	int str_index = 0;
	while (str[str_index] != 0) {
		target[target_index] = str[str_index];
		target_index++;
		str_index++;
	}

	return str_index;
}

static int print_int(char *target, int number)
{
	int length = 0;
	if (number < 0) {
		target[0] = '-';
		length = 1;
		length += print_uint_in_base(target + 1, -number, 10);
	} else {
		length = print_uint_in_base(target, number, 10);
	}

	return length;
}

static int print_uint_in_base(char *target, uint32_t number, uint32_t base)
{
	int length = 0;
	uint32_t last_digit = 0;
	uint32_t rest = 0;

	rest = unsigned_divmod(number, base, &last_digit);
	if (rest != 0)
		length = print_uint_in_base(target, rest, base);

	target[length] = DIGITS[last_digit];
	length++;

	return length;
}

/*
 * unsigned_divmod divides numerator and denmoriator, then returns the quotient
 * as result. If remainder_pointer is not NULL, then the function returns the
 * division remainder in remainder_pointer.
 *
 * Algorithm: http://en.wikipedia.org/wiki/Division_algorithm
 */
uint32_t unsigned_divmod(uint32_t numerator, uint32_t denominator,
			 uint32_t* remainder_pointer)
{
	int i = 0;
	uint32_t quotient = 0;
	uint32_t remainder = 0;

	for (i = 31; i >= 0; i--) {
		uint32_t numerator_bit = ((numerator & (1u << i)) ? 1 : 0);
		
		remainder = (remainder << 1) | numerator_bit;
		if (remainder >= denominator) {
			remainder -= denominator;
			quotient |= (1u << i);
		}
	}

	if (remainder_pointer)
		(*remainder_pointer) = remainder;

	return quotient;
}