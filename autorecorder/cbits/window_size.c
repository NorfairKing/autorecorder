#include <sys/ioctl.h>

unsigned long c_get_window_size(int fd) {
    struct winsize w;
    if (ioctl (fd, TIOCGWINSZ, &w) >= 0)
        return (w.ws_row << 16) + w.ws_col;
    else
        return 0x190050;
}

void c_set_window_size(int fd, unsigned short rows, unsigned short cols) {
    struct winsize w;
    if (ioctl(fd, TIOCGWINSZ, &w) >= 0) {
        w.ws_row = rows;
        w.ws_col = cols;
        ioctl(fd, TIOCSWINSZ, &w);
    }
}
