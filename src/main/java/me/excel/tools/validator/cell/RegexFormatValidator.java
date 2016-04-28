package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.ExcelCell;

/**
 * Created by hanwen on 15-12-16.
 */
public class RegexFormatValidator extends AbstractCellValidator {

  /**
   * regex statement
   */
  protected String regex;

  public RegexFormatValidator(String field, String regex) {
    super(field, "格式应该满足:"+regex, regex);
    this.regex = regex;
  }

  public RegexFormatValidator(String field, String regex, String message, String prompt) {
    super(field, message, prompt);
    this.regex = regex;
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {
    return excelCell.getValue().matches(regex);
  }
}
