package me.excel.tools.utils;

/**
 * Created by hanwen on 15-12-18.
 */
public class BooleanTranslator {

  private BooleanTranslator() {
  }

  public static boolean parseBoolean(String value) {
    if (isValidTrue(value)) {
      return Boolean.TRUE;
    } else if (isValidFalse(value)) {
      return Boolean.FALSE;
    }
    return Boolean.parseBoolean(null);
  }

  public static boolean isValidTrue(String value) {
    if ("是".equals(value) || "yes".equalsIgnoreCase(value) ||
        "y".equalsIgnoreCase(value) || "1".equals(value) ||
        "true".equalsIgnoreCase(value) || "t".equalsIgnoreCase(value)) {
      return true;
    }
    return false;
  }

  public static boolean isValidFalse(String value) {
    if ("否".equals(value) || "no".equalsIgnoreCase(value) ||
        "n".equalsIgnoreCase(value) || "0".equals(value) ||
        "false".equalsIgnoreCase(value) || "f".equalsIgnoreCase(value)) {
      return true;
    }
    return false;
  }
}
