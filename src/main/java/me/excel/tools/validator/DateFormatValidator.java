package me.excel.tools.validator;

import me.excel.tools.model.excel.ExcelCell;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Created by hanwen on 15-12-16.
 */
public class DateFormatValidator extends AbstractFieldValidator {

  protected String format;

  public DateFormatValidator(String field, String format) {
    super(field, "数据不正确,格式应该为:"+format, format);
    this.format = format;
  }

  public DateFormatValidator(String field, String format, String errorMessage, String prompt) {
    super(field, errorMessage, prompt);
    this.format = format;
  }

  @Override
  public boolean validate(ExcelCell excelCell) {
    String value = excelCell.getValue();
    if (value == null) {
      return false;
    }
    return isValidFormat(format, value);
  }

  private boolean isValidFormat(String format, String value) {
    Date date;
    try {
      SimpleDateFormat sdf = new SimpleDateFormat(format);
      date = sdf.parse(value);
      if (!value.equals(sdf.format(date))) {
        date = null;
      }
    } catch (ParseException e) {
      return false;
    }
    return date != null;
  }
}
