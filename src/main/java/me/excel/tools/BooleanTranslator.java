package me.excel.tools;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * translate human boolean to model value.
 * <p>
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
    return ArrayUtils.contains(new String[]{"是", "yes", "y", "1", "true", "t"}, StringUtils.lowerCase(value));
  }

  public static boolean isValidFalse(String value) {
    return ArrayUtils.contains(new String[]{"否", "no", "n", "0", "false", "f"}, StringUtils.lowerCase(value));
  }
}
