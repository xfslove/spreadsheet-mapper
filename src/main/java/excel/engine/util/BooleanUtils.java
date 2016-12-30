package excel.engine.util;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * excel engine use boolean w2f
 * <p>
 * Created by hanwen on 15-12-18.
 */
public class BooleanUtils {

  private BooleanUtils() {
    // default constructor
  }

  /**
   * convert from string to boolean
   *
   * @param value string value
   * @return boolean value
   */
  public static Boolean stringToBoolean(String value) {
    if (isValidTrue(value)) {
      return Boolean.TRUE;
    } else if (isValidFalse(value)) {
      return Boolean.FALSE;
    }
    return null;
  }

  /**
   * convert from boolean to zh string value
   *
   * @param value boolean value
   * @return string value
   */
  public static String booleanToZhString(Object value) {
    if (value instanceof Boolean) {
      if (Boolean.TRUE.equals(value)) {
        return "是";
      } else if (Boolean.FALSE.equals(value)) {
        return "否";
      }
    }
    return null;
  }

  /**
   * is string valid true
   *
   * @param value string value
   * @return true if valid
   */
  public static boolean isValidTrue(String value) {
    return ArrayUtils.contains(new String[]{"是", "yes", "y", "1", "true", "t"}, StringUtils.lowerCase(value));
  }

  /**
   * is string value false
   *
   * @param value string value
   * @return false if valid
   */
  public static boolean isValidFalse(String value) {
    return ArrayUtils.contains(new String[]{"否", "no", "n", "0", "false", "f"}, StringUtils.lowerCase(value));
  }
}
