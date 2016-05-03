package me.excel.tools.model;

import org.apache.commons.lang3.StringUtils;

import java.util.HashMap;
import java.util.Map;

import static me.excel.tools.ExcelConstants.*;

/**
 * Created by hanwen on 5/3/16.
 */
public class ExcelSupportedDateFormat {

  private ExcelSupportedDateFormat() {
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

  public static void registerFormat(String excelDateFormat, String dateFormat) {
    EXCEL_SUPPORTED_DATE_FORMAT.put(excelDateFormat, dateFormat);
  }

  public static String getDateFormat(String format) {
    return EXCEL_SUPPORTED_DATE_FORMAT.get(StringUtils.lowerCase(format));
  }
}
