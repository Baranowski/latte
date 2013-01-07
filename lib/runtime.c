#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main__();

void printInt(int x) {
    printf("%d\n", x);
}

void printString(char *str) {
    printf("%s\n", str);
}

void error(char *str) {
    fprintf(stderr, "%s\n", str);
}


#define STR_BUF_SIZE 100
char *readString() {
    char *res;
    int l;
    res = malloc(STR_BUF_SIZE);
    fgets(res, STR_BUF_SIZE, stdin);
    l = strlen(res);
    res[l-1] = '\0';
    return res;
}

int readInt() {
    char *str;
    int l;
    str = readString();
    l = atoi(str);
    free(str);
    return l;
}

int strComp(char *s1, char *s2) {
    return (strcmp(s1, s2) < 0);
}

char *strConcat(char *s1, char *s2) {
    int l1,l2;
    char *res;
    l1 = strlen(s1);
    l2 = strlen(s2);
    res = malloc(l1+l2+1);
    strncpy(res, s1, l1);
    strncpy(res+l1, s2, l2+1);
    return res;
}

int main(int argc, char **argv) {
    return main__();
}
