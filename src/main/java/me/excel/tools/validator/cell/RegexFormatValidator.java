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

  public RegexFormatValidator(String matchField, String regex) {
    super(matchField, "格式应该满足:" + regex);
    this.regex = regex;
  }

  public RegexFormatValidator(String matchField, String regex, String errorMessage) {
    super(matchField, errorMessage);
    this.regex = regex;
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {
    return excelCell.getValue().matches(regex);
  }
}
