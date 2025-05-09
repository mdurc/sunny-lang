- [x] Make more integer types with specific bit counts via c-ints (this is actually something to implement code gen because we already have type specifiers)
- [ ] Multi-line comments /\* \*/
- [ ] Tree sitter highlighting
- [ ] Type checker
- [ ] Add pointers
- [ ] Synchronize function for handling multiple parsing errors instead of crashing on the first one
```
#define PARSE_OR_BAIL(fn_call) \
    ({ \
        ASTNode* _result = (fn_call); \
        if (p->panic_mode) return NULL; \
        _result; \
    })
```

