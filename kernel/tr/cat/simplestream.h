#ifndef SIMPLE_STREAM
#define SIMPLE_STREAM

#define SSTREAM_SAVED_LENGTH ((size_t) -1)

class se_simplestream {
protected:
    size_t m_capacity;
    size_t m_length;
    off_t  m_position;
    size_t m_save_str_len;
    void * m_content;

    void grow (size_t garanteed_size = 128) {
        m_capacity = (((garanteed_size >> 6) + 1) << 6);
        m_content = realloc(m_content, m_capacity);
    };

public:

    se_simplestream() : m_capacity(0), m_length(0), m_position(0), m_content(NULL) {
        m_capacity = 256;
        m_content = malloc(m_capacity);
    };

    ~se_simplestream() { free(m_content); };

    inline void write(const void * s, size_t length) {
        if (m_position + length > m_capacity) { grow(m_position + length); };
        if (m_position + length > m_length)   { m_length = m_position + length; };

        memcpy(((char *) m_content + m_position), s, length);
        m_position += length;
    };

/**
 *  String are stored in the following way: In first bytes the string length is stored.
 *  It can occupy 1, 2 or sizeof(size_t) bytes.
 *  If length is less then 255, it is stored in 1 byte
 *  otherwise, it is stored in sizeof(size_t) bytes 
 *  (actually, it should be error)

 *  Length is counted without trailing null character, but in case, 
 *  when the string countains only it, it is counted.
 */

    void write_string(const char * s) {
        if (s == NULL) {
            write_char('\0');
        } else {
            size_t len = strlen(s);
            uint8_t a;

            if (len == 0) {
                a = 1;
                write(&a, sizeof(uint8_t));
                a = 0;
                write(&a, sizeof(uint8_t));
            } else if (len < 0x0ff) {
                a = (uint8_t) len;
                write(&a, sizeof(uint8_t));
                write(s, len);
            } else {
                a = 0x0ff;
                write(&a, sizeof(uint8_t));
                write(&len, sizeof(size_t));
                write(s, len);
            }
        }
    };

/*
  Returns space, actually needed to store string. I.e. with trailing 
  terminating null
 */

    size_t read_string_len() {
        uint8_t a;
        read(&a, sizeof(uint8_t));

        if (a == 0) {
            return (m_save_str_len = 0);
        } else if ((a == 1) && (*get_content() == '\0')) {
            seek(1); /* read trailing zero, so we do not need to read string at all */
            return (m_save_str_len = 1);
        } else if (a < 255) {
            return (m_save_str_len = a + 1);
        } else {
            size_t len;
            read(&len, sizeof(size_t));
            return (m_save_str_len = len + 1);
        }
    };

/*
  This function works only in assertion, that read_string_len 
  was called previously. 
  if len parameter is SSTREAM_SAVED_LENGTH, saved length from prvious call is used
 */
    void read_string(size_t len, char * c) {
        if (len == SSTREAM_SAVED_LENGTH) { len = m_save_str_len; }
        if (len == 0) {
          U_ASSERT(c == NULL);
        } else {
          if (len > 1) read(c, len - 1);
          c[len-1] = '\0';
        }
    };

    void write_char(const char s) {
        if (m_position + 1 > m_capacity) { grow(m_position + 1); };
        if (m_position + 1 > m_length)   { m_length = m_position + 1; };

        *((char *) m_content + m_position) = s;
        m_position += 1;
    };

    bool read(void * s, size_t length) {
        U_ASSERT(m_position + length <= m_length);

        memcpy(s, ((char *) m_content + m_position), length);
        m_position += length;

        return true;
    };

    inline char * get_content() { return ((char *) m_content + m_position); };

    inline void seekto(char stopat = '\0') { 
        char * cin  = (char *) m_content + m_position;
        size_t len = m_length - m_position, i = 0;

        while (i < len && *cin != stopat) { ++i; ++cin; };
        if (i < len) { ++i; }

        m_position += i;
    };

    inline void seek(off_t d) { m_position += d; };
    inline void absseek(off_t d) { m_position = d; };

    inline void set_length(size_t new_length) { if (new_length > m_capacity) { grow(new_length); }; m_length = new_length; };
    inline size_t get_length() const { return m_length; };

    inline void set_capacity(size_t new_capacity) { grow(new_capacity); };
    inline size_t get_capacity() const { return m_capacity; };
};

#endif // SIMPLE_STREAM
