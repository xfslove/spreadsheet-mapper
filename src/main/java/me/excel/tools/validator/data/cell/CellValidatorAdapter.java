package me.excel.tools.validator.data.cell;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.message.DataValidateMessage;
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
  public String getKey() {
    return matchField;
  }


  @Override
  public DataValidateMessage validate(ExcelCell excelCell) {
    boolean blank = StringUtils.isBlank(excelCell.getValue());

    if (blank) {
      return DataValidateMessage.SUCCESS;
    }

    return customValidate(excelCell);
  }

  /**
   * for customer access errorMessage
   *
   * @param errorMessage error message
   */
  protected void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  protected final String getMatchField() {
    return matchField;
  }

  protected abstract DataValidateMessage customValidate(ExcelCell excelCell);

}
