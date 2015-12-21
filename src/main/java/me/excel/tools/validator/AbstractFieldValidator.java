package me.excel.tools.validator;

import me.excel.tools.model.excel.ExcelCell;

/**
 * 自定义的field validator需继承此类
 *
 * Created by hanwen on 15-12-16.
 */
public abstract class AbstractFieldValidator implements FieldValidator {

  protected String matchField;

  /**
   * 错误提示信息
   */
  protected String errorMessage;

  /**
   * 提示信息
   */
  protected String prompt;

  public AbstractFieldValidator(String matchField, String errorMessage, String prompt) {
    this.matchField = matchField;
    this.errorMessage = errorMessage;
    this.prompt = prompt;
  }

  @Override
  public abstract boolean validate(ExcelCell excelCell);

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }

  @Override
  public String getPrompt() {
    return this.prompt;
  }

  @Override
  public boolean matches(ExcelCell cell) {
    return cell.getField().equals(matchField);
  }

  @Override
  public boolean matches(String field) {
    return field.equals(matchField);
  }

}
