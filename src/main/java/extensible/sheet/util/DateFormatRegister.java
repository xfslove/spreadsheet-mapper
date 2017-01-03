package extensible.sheet.util;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * register support date format when excel date format value read to object field
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class DateFormatRegister {

  public static final String FULL_LOCAL_DATE_PATTERN = "yyyy-MM-dd";

  public static final String LOCAL_DATE_WITH_YEAR_MONTH_PATTERN = "yyyy-MM";

  public static final String LOCAL_DATE_WITH_YEAR_PATTERN = "yyyy";

  public static final String FULL_LOCAL_DATE_TIME_PATTERN = "yyyy-MM-dd HH:mm:ss";

  public static final String LOCAL_DATE_TIME_WITH_HOUR_MIN_PATTERN = "yyyy-MM-dd HH:mm";

  public static final String LOCAL_DATE_TIME_WITH_HOUR_PATTERN = "yyyy-MM-dd HH";

  public static final String ERROR_PATTERN = "error-date-pattern";

  private DateFormatRegister() {
    // default constructor
  }

  private static final Map<String, String> DATE_FORMAT_CORRESPONDING = new ConcurrentHashMap<>();

  static {
    register("yyyy", LOCAL_DATE_WITH_YEAR_PATTERN);
    register("yyyy\\-mm", LOCAL_DATE_WITH_YEAR_MONTH_PATTERN);
    register("yyyy\\-mm\\-dd", FULL_LOCAL_DATE_PATTERN);
    register("yyyy\\-mm\\-dd\\ HH", LOCAL_DATE_TIME_WITH_HOUR_PATTERN);
    register("yyyy\\-mm\\-dd\\ HH:mm", LOCAL_DATE_TIME_WITH_HOUR_MIN_PATTERN);
    register("yyyy\\-mm\\-dd\\ HH:mm:ss", FULL_LOCAL_DATE_TIME_PATTERN);
  }

  /**
   * register customer format
   *
   * @param customerFormat date format of customer definition
   * @param dateFormat     date format of object
   */
  public static void register(String customerFormat, String dateFormat) {
    DATE_FORMAT_CORRESPONDING.put(customerFormat, dateFormat);
  }

  public static String get(String customerFormat) {
    return DATE_FORMAT_CORRESPONDING.get(customerFormat);
  }
}
