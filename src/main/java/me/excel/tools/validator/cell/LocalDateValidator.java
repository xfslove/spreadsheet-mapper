package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.validator.SkipValidateException;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

/**
 * Created by hanwen on 5/3/16.
 */
public class LocalDateValidator extends AbstractCellValidator {

  public LocalDateValidator(String field) {
    super(field, "格式应该为: yyyy-MM-dd", "yyyy-MM-dd");
  }

  public LocalDateValidator(String field, String errorMessage, String prompt) {
    super(field, errorMessage, prompt);
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) throws SkipValidateException {

    DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd");

    try {
      dateTimeFormatter.parseLocalDate(excelCell.getValue());
    } catch (IllegalArgumentException e) {
      return false;
    }
    return true;
  }
}
