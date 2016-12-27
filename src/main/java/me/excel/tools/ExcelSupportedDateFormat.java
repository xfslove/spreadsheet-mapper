package me.excel.tools;

import org.apache.commons.lang3.StringUtils;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * support date format when excel value read to object field
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class ExcelSupportedDateFormat {

  public static final String FULL_LOCAL_DATE_PATTERN = "yyyy-MM-dd";

  public static final String LOCAL_DATE_WITH_YEAR_MONTH_PATTERN = "yyyy-MM";

  public static final String LOCAL_DATE_WITH_YEAR_PATTERN = "yyyy";

  public static final String FULL_LOCAL_DATE_TIME_PATTERN = "yyyy-MM-dd HH:mm:ss";

  public static final String LOCAL_DATE_TIME_WITH_HOUR_MIN_PATTERN = "yyyy-MM-dd HH:mm";

  public static final String LOCAL_DATE_TIME_WITH_HOUR_PATTERN = "yyyy-MM-dd HH";

  public static final String ERROR_PATTERN = "error-date-pattern";

  private ExcelSupportedDateFormat() {
    // default constructor
  }

  private static final Map<String, String> EXCEL_SUPPORTED_DATE_FORMAT = new HashMap<>();

  static {
    registerFormat("yyyy", LOCAL_DATE_WITH_YEAR_PATTERN);
    registerFormat("yyyy\\-mm", LOCAL_DATE_WITH_YEAR_MONTH_PATTERN);
    registerFormat("yyyy\\-mm\\-dd", FULL_LOCAL_DATE_PATTERN);
    registerFormat("yyyy\\-mm\\-dd\\ HH", LOCAL_DATE_TIME_WITH_HOUR_PATTERN);
    registerFormat("yyyy\\-mm\\-dd\\ HH:mm", LOCAL_DATE_TIME_WITH_HOUR_MIN_PATTERN);
    registerFormat("yyyy\\-mm\\-dd\\ HH:mm:ss", FULL_LOCAL_DATE_TIME_PATTERN);
  }

  /**
   * register customer format
   *
   * @param excelDateFormat date format of excel cell
   * @param dateFormat      date format of object
   */
  public static void registerFormat(String excelDateFormat, String dateFormat) {
    EXCEL_SUPPORTED_DATE_FORMAT.put(excelDateFormat, dateFormat);
  }

  public static String getDateFormat(String format) {
    return EXCEL_SUPPORTED_DATE_FORMAT.get(StringUtils.lowerCase(format));
  }

  public static Collection<String> getSupportedFormats() {
    return EXCEL_SUPPORTED_DATE_FORMAT.values();
  }
}
