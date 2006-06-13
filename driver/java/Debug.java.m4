
package ru.ispras.sedna.driver;

class Debug {
    public static final boolean DEBUG = ifelse(EL_DEBUG, `1', true, false);
}
