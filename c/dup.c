#define _DEFAULT_SOURCE

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/sendfile.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <unistd.h>

void usage(const char* exec)
{
    printf ("\nUsage: %s [-c NUM] IN_FILE [OUT_FILE]\n"
            "Generate OUT_FILE whose content is duplicated with NUM times of IN_FILE.\n"
            "   IN_FILE:  Path of input file.\n"
            "   OUT_FILE: Path of output file (It will be the same as IN_FILE by default).\n"
            "   -c NUM:   Number of copies (1 by default).\n"
            ,
            exec);
}

#define USAGE()  \
    do { usage(argv[0]); exit(EXIT_FAILURE); } while (0)

#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

#if !defined (PDEBUG)
#define PDEBUG(fmt, ...)                                              \
    do {                                                              \
        fprintf(stderr, "app: - %s:%d -- %s: ",                       \
                __FILE__, __LINE__,__FUNCTION__);                     \
        fprintf(stderr, fmt, ##  __VA_ARGS__);                        \
    } while(0)
#endif  /*End of if PDEBUG*/

#define K       (1 << 10)
#define M       (1 << 20)
#define G       (1 << 30)
#define T       (1 << 40)

const char *stringify_size(size_t sz)
{
    static char str_size[64] = { '\0' };
    memset(str_size, 0, 64);

    if (sz < K) {
        sprintf(str_size, "%zu", sz);
    }
    else if (sz < M) {
        sprintf(str_size, "%.02fK", (double) sz / K);
    } else if (sz < G) {
        sprintf(str_size, "%.02fM", (double) sz / M);
    } else {
        sprintf(str_size, "%.02fG", (double) sz / G);
    }

    return str_size;
}

size_t file_enlarge(int in_fd, int num)
{
    struct stat st;
    if (fstat(in_fd, &st) == -1) {
        handle_error("Failed to get file status");
    }

    size_t size = st.st_size * (num + 1);
    if (ftruncate(in_fd, size) == -1) {
        handle_error("Failed to truncate file");
    }

    void* addr = mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, in_fd, 0);
    if (!addr) {
        handle_error("Failed to mmap()");
    }

    int done = 1;
    char* dst = (char*)addr;
    do {
        dst += st.st_size;
        memcpy(dst, addr, st.st_size);
    } while (done++ < num);

    munmap(addr, size);

    return dst - (char*)addr;
}

size_t copy_file(int in_fd, const char* out_file, int num)
{
    struct stat st;
    if (fstat(in_fd, &st) == -1) {
        handle_error("Failed to get file status");
    }

    int ofd = open(out_file, O_WRONLY|O_CREAT, S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH);
    if (ofd == -1) {
        handle_error("Failed to open output file");
    }

    size_t total = 0;
    while (num-- > 0) {
        size_t copied = 0;
        if ((copied = sendfile(ofd, in_fd, NULL, st.st_size)) == -1) {
            close(ofd);
            handle_error("Failed to send file..");
        }
        lseek(in_fd, 0, SEEK_SET);
        total += copied;
    }

    close(ofd);

    return total;
}

int main(int argc, char *argv[])
{
    int opt, in_fd, num = 1;
    size_t total;

    if (argc < 2) {
        USAGE();
    }

    while ((opt = getopt(argc, argv, "c:")) != -1) {
        switch (opt) {
        case 'c': {
            num = atoi(optarg);
            if (num < 1) {
                fprintf(stderr, "Number (%d) should be be euqal or greater than 1\n", num);
                USAGE();
            }
            break;
        }
        default: {
            break;
        }
        }
    }

    if (optind >= argc) {
        fprintf(stderr, "Expected argument after options.\n");
        USAGE();
        return 1;
    }

    if ((in_fd = open(argv[optind], O_RDWR)) == -1) {
        handle_error("Failed to open input file");
    }

    if (optind + 1 >= argc) { // out_file not specified.
        total = file_enlarge(in_fd, num);
    }
    else {
        total = copy_file(in_fd, argv[optind+1], num);
    }

    close(in_fd);

    if (total) {
        printf ("Operation succeeded, %s bytes copied.\n", stringify_size(total));
        return 0;
    }
    else {
        printf ("Operation failed.\n");
    }

    return 1;
}
