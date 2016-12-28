package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.Cell;
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
  public int getSheetIndex() {
    return 1;
  }


  @Override
  public String getKey() {
    return matchField;
  }

  @Override
  public String getMatchField() {
    return matchField;
  }

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }

  @Override
  public boolean validate(Cell cell) {
    return StringUtils.isBlank(cell.getValue()) || customValidate(cell);
  }

  /**
   * for customer access errorMessage
   *
   * @param errorMessage error message
   */
  protected void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  protected abstract boolean customValidate(Cell cell);

}
