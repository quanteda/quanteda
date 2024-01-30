
int utf8_length(const std::string &text) {
    int n = 0;
    size_t i = 0;
    while (i < text.length()) {
        int cplen = 0;
        if ((text[i] & 0xf8) == 0xf0) {
            cplen = 4;   
        } else if ((text[i] & 0xf0) == 0xe0) {
            cplen = 3;   
        } else if ((text[i] & 0xe0) == 0xc0) {
            cplen = 2;
        } else if ((text[i] & 0x80) == 0) {
            cplen = 1;
        }
        if (cplen > 0) {
            n++;
        }
        i += cplen;
    }
    return(n);
}

std::string utf8_sub_left(const std::string &text, const int len = 0) {
    int n = 0;
    size_t i = 0;
    while (i < text.length()) {
        int cplen = 0;
        if ((text[i] & 0xf8) == 0xf0) {
            cplen = 4;   
        } else if ((text[i] & 0xf0) == 0xe0) {
            cplen = 3;   
        } else if ((text[i] & 0xe0) == 0xc0) {
            cplen = 2;
        } else if ((text[i] & 0x80) == 0) {
            cplen = 1;
        }
        if (cplen > 0) {
            n++;
            //Rcout << i << " " << n << " " << cplen << ": "<< text.substr(i, cplen) << "\n";
        }
        if (n > len)
            return text.substr(0, i);
        i += cplen;
    }
    return text;
}

std::string utf8_sub_right(const std::string &text, const int len = 0) {
    int n = 0;
    size_t i = text.length();
    while (0 < i) {
        int cplen = 0;
        if ((text[i] & 0xf8) == 0xf0) {
            cplen = 4;   
        } else if ((text[i] & 0xf0) == 0xe0) {
            cplen = 3;   
        } else if ((text[i] & 0xe0) == 0xc0) {
            cplen = 2;
        } else if ((text[i] & 0x80) == 0) {
            cplen = 1;
        }
        if (cplen > 0) {
            n++;
            //Rcout << i << " " << n << " / " << len << " " << ": "<< text.substr(i, cplen) << "\n";
        }
        if (n > len)
            return text.substr(i);
        i -= 1;
    }
    return text;
}
