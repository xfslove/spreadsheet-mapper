package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCell;
import org.apache.commons.lang3.StringUtils;

/**
 * <pre>
 * cell value validator adapter, easy implements customer value validator extends this.
 * extends this validator will skip valid when cell value is blank.
 * </pre>
 * Created by hanwen on 15-12-16.
 */
public abstract class CellValidatorAdapter implements CellValidator {

  private String matchField;

  private String errorMessage;

  public CellValidatorAdapter(String matchField, String errorMessage) {
    this.matchField = matchField;
    this.errorMessage = errorMessage;
  }

  @Override
  public boolean validate(ExcelCell excelCell) {
    return StringUtils.isBlank(excelCell.getValue()) || customValidate(excelCell);
  }

  protected final String getMatchField() {
    return matchField;
  }

  protected abstract boolean customValidate(ExcelCell excelCell);

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }

  @Override
  public boolean matches(String field) {
    return field.equals(matchField);
  }

}
