#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef NDEBUG
#define DBG 1
#else
#define DBG 0
#endif

int main__();

struct myStr {
    long refCnt;
    char *str;
};

void __strRef(struct myStr *s) {
    if (s != NULL && s->refCnt != (long)0xFFFFFFFF) {
        if (DBG) {
            fprintf(stderr, "__strRef: %s, refCnt: %ld\n", s->str, s->refCnt);
        }
        s->refCnt++;
    }
}

void __strUnref(struct myStr *s) {
    if (s != NULL && s->refCnt != (long)0xFFFFFFFF) {
        if (DBG) {
            fprintf(stderr, "__strUnref: %s, refCnt: %ld\n", s->str, s->refCnt);
        }
        s->refCnt--;
        if (s->refCnt == 0) {
            if (DBG) {
                fprintf(stderr, "free\n");
            }
            free(s->str);
            free(s);
        }
    }
}

void printInt(long int x) {
    printf("%ld\n", x);
}

void printString(struct myStr *str) {
    printf("%s\n", str->str);
    __strUnref(str);
}

void error(struct myStr *str) {
    fprintf(stderr, "%s\n", str->str);
    __strUnref(str);
}


#define STR_BUF_SIZE 100
struct myStr *readString() {
    struct myStr *res;
    int l;
    res = malloc(sizeof(struct myStr));
    res->str = malloc(STR_BUF_SIZE);
    fgets(res->str, STR_BUF_SIZE, stdin);
    l = strlen(res->str);
    res->str[l-1] = '\0';
    res->refCnt = 1;
    return res;
}

int readInt() {
    struct myStr *str;
    int l;
    str = readString();
    l = atoi(str->str);
    __strUnref(str);
    return l;
}

int strComp(struct myStr *s1, struct myStr *s2) {
    int res = (strcmp(s1->str, s2->str) < 0);
    __strUnref(s1);
    __strUnref(s2);
}

struct myStr *strConcat(struct myStr *s1, struct myStr *s2) {
    int l1,l2;
    char *resStr;
    struct myStr *res;
    l1 = strlen(s1->str);
    l2 = strlen(s2->str);
    resStr = malloc(l1+l2+1);
    res = malloc(sizeof(struct myStr));
    strncpy(resStr, s1->str, l1);
    strncpy(resStr+l1, s2->str, l2+1);
    res->str = resStr;
    res->refCnt = 1;
    __strUnref(s1);
    __strUnref(s2);
    return res;
}

int main(int argc, char **argv) {
    return main__();
}


