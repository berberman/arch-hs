#ifndef CLIB_H
#define CLIB_H

typedef void callback_t(const char *, const char *);

void query_community(callback_t callback);

void query_files(const char *dbname, callback_t callback);

#endif