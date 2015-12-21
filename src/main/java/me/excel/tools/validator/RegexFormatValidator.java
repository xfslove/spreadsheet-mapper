package me.excel.tools.validator;

import me.excel.tools.model.excel.ExcelCell;

/**
 * Created by hanwen on 15-12-16.
 */
public class RegexFormatValidator extends AbstractFieldValidator {

  /**
   * regex statement
   */
  protected String regex;

  public RegexFormatValidator(String field, String regex) {
    super(field, "格式不正确, 格式应该满足:"+regex, regex);
    this.regex = regex;
  }

  public RegexFormatValidator(String field, String regex, String message, String prompt) {
    super(field, message, prompt);
    this.regex = regex;
  }

  @Override
  public boolean validate(ExcelCell excelCell) {
    String value = excelCell.getValue();
    if (value == null) {
      return false;
    }
    return value.matches(regex);
  }
}
