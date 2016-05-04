package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.validator.SkipValidateException;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

/**
 * Created by hanwen on 5/3/16.
 */
public class LocalDateValidator extends AbstractCellValidator {

  private String format;

  public LocalDateValidator(String field, String format) {
    super(field, "格式应该为: " + format, format);
    this.format = format;
  }

  public LocalDateValidator(String field, String format, String errorMessage, String prompt) {
    super(field, errorMessage, prompt);
    this.format = format;
  }
  @Override
  protected boolean customValidate(ExcelCell excelCell) {

    DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern(format);

    try {
      dateTimeFormatter.parseLocalDate(excelCell.getValue());
    } catch (IllegalArgumentException e) {
      return false;
    }
    return true;
  }
}
