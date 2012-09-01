#ifndef _WARDEN_H_
#define _WARDEN_H_

template <typename BorderType>
struct Warden
{
    BorderType base;
    bool started;

    void start() {
        base.start();
        started = true;
    }

    void stop() {
        if (started) {
            stop();
        };
    };

    Warden() : started(false) {
        start();
    };

    ~Warden() {
        stop();
    };
};

#endif /* _WARDEN_H_ */
