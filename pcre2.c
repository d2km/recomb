/*
  compile it with
  $ cc -O2 -lpcre2-8 pcre2.c -o pcre2
  and run as
  $ ./pcre2 < sources.txt > /dev/null
*/
#define NUMBER_OF_RUNS 1000
#define PCRE2_CODE_UNIT_WIDTH 8

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <pcre2.h>
#include <time.h>

void replace_all(
     pcre2_code *re,
     pcre2_match_data *md,
     const char * source,
     size_t source_len
     )
{
     int rc;
     const char *s = source;
     size_t len = source_len;

     while(1) {
          if (len == 0) {
               fprintf(stdout, "\n");
               return;
          }

          rc = pcre2_match(re, (PCRE2_SPTR)s, len, 0, 0, md, NULL);

          if (rc == PCRE2_ERROR_NOMATCH) {
               fprintf(stdout, "%s\n", s);
               return;
          } else if (rc < 0) {
               fprintf(stderr, "Matching error %d\n", rc);
               exit(1);
          } else {
               PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(md);
               int start = (int)ovector[0];
               int end = (int)ovector[1];

               fwrite(s, start, 1, stdout);
               fprintf(stdout, "%s", "_");
               s = s + end;
               len = len - end;
          }
     }
}

int main(int argc, char **argv)
{
     pcre2_code *re;
     int errnum, i = 0, j = 0, sources_len = 0;
     PCRE2_SIZE erroff;
     pcre2_match_data *match_data;
     char buf[512] = {0};
     char* sources[1024] = {0};
     clock_t start, end;
     float ms;

     re = pcre2_compile((PCRE2_SPTR)"[/:\\\\. (),'\"]+",
                        PCRE2_ZERO_TERMINATED,
                        0,
                        &errnum,
                        &erroff,
                        NULL);

     assert(re != NULL);

     /* that improves performance by 30-40% */
     pcre2_jit_compile(re, PCRE2_JIT_COMPLETE);

     match_data = pcre2_match_data_create_from_pattern(re, NULL);

     /* read all the data into sources array */
     while(1) {
          char *p = buf;
          int eof = 0;
          while(1) {
               int c = fgetc(stdin);
               if (c == EOF) {
                    *p = 0; eof = 1; break;
               } else if (c == '\n') {
                    *p = 0; break;
               }
               *p++ = (char)c;
          }
          sources[i++] = strndup(buf, 256);
          if (eof == 1 || i == sizeof(sources)) {
               sources_len = i;
               break;
          }
     }

     /* run it NUMBER_OF_RUNS times */
     start = clock();
     for(j = 0; j < NUMBER_OF_RUNS; j++) {
          for(i = 0; i < sources_len; i++)
               replace_all(re, match_data, sources[i], strlen(sources[i]));
     }
     end = clock();
     ms = (float)(end - start)/((CLOCKS_PER_SEC/1000) * NUMBER_OF_RUNS);
     fprintf(stderr, "run time %f ms\n", ms);

     pcre2_match_data_free(match_data);
     for(i = 0; i < sources_len; i++)
          free(sources[i]);
     return 0;
}
