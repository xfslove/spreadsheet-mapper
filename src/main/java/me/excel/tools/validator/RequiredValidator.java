package me.excel.tools.validator;

import me.excel.tools.model.excel.ExcelCell;
import org.apache.commons.lang3.StringUtils;

/**
 * Created by hanwen on 15-12-16.
 */
public class RequiredValidator implements FieldValidator {

  protected String matchField;

  protected String errorMessage;

  protected String prompt;

  public RequiredValidator(String matchField) {
    this.matchField = matchField;
    this.errorMessage = "应该为必填";
    this.prompt = "必填";
  }

  public RequiredValidator(String matchField, String errorMessage, String prompt) {
    this.matchField = matchField;
    this.errorMessage = errorMessage;
    this.prompt = prompt;
  }

  @Override
  public String getErrorMessage() {
    return this.errorMessage;
  }

  @Override
  public String getPrompt() {
    return prompt;
  }

  @Override
  public boolean validate(ExcelCell excelCell) {
    return StringUtils.isNotBlank(excelCell.getValue());
  }

  @Override
  public boolean matches(ExcelCell excelCell) {
    return excelCell.getField().equals(matchField);
  }

  @Override
  public boolean matches(String field) {
    return field.equals(matchField);
  }
}
