package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.ExcelCell;

/**
 * regex format validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class RegexFormatValidator extends CellValidatorAdapter {

  /**
   * regex statement
   */
  protected String regex;

  public RegexFormatValidator(String field, String regex) {
    super(field, "格式应该满足:" + regex);
    this.regex = regex;
  }

  public RegexFormatValidator(String field, String regex, String message) {
    super(field, message);
    this.regex = regex;
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {
    return excelCell.getValue().matches(regex);
  }
}
