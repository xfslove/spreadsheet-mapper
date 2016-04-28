package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.ExcelCell;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Created by hanwen on 15-12-16.
 */
public class DateFormatValidator extends AbstractCellValidator {

  protected String format;

  public DateFormatValidator(String field, String format) {
    super(field, "格式应该为:"+format, format);
    this.format = format;
  }

  public DateFormatValidator(String field, String format, String errorMessage, String prompt) {
    super(field, errorMessage, prompt);
    this.format = format;
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {
    return isValidFormat(format, excelCell.getValue());
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
