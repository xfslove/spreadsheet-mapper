package me.excel.tools.validator.data.cell;

import me.excel.tools.model.excel.ExcelCell;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

/**
 * local date time validator
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LocalDateTimeValidator extends CellValidatorAdapter {

  private String format;

  public LocalDateTimeValidator(String matchField, String format) {
    super(matchField, "格式应该为: " + format);
    this.format = format;
  }

  public LocalDateTimeValidator(String matchField, String format, String errorMessage) {
    super(matchField, errorMessage);
    this.format = format;
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {
    DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern(format);

    try {
      dateTimeFormatter.parseLocalDateTime(excelCell.getValue());
    } catch (IllegalArgumentException e) {
      return false;
    }
    return true;
  }
}
