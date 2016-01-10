package me.excel.tools.validator;

import me.excel.tools.model.excel.ExcelCell;
import org.apache.commons.lang3.StringUtils;

/**
 * model的field validator父类, 提供默认的validator,<br/>
 * 默认的validator 支持{@link java.util.Date}, {@link Integer}, {@link Double}, {@link Boolean},<br/>
 * 自定义validator 用 {@link CommonValidator}
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
  public boolean validate(ExcelCell excelCell) {
    if (StringUtils.isBlank(excelCell.getValue())) {
      return true;
    }
    return customValidate(excelCell);
  }

  protected abstract boolean customValidate(ExcelCell excelCell);

  @Override
  public String getErrorMessage() {
    return this.errorMessage;
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
