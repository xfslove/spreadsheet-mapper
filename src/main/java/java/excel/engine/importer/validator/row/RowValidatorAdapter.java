package java.excel.engine.importer.validator.row;


import java.excel.engine.model.excel.Row;

import java.util.HashSet;
import java.util.Set;

/**
 * row values validator adapter, easy implements customer value validator extends this.
 * <p>
 * Created by hanwen on 4/26/16.
 */
public abstract class RowValidatorAdapter implements RowValidator {

  private int sheetIndex;

  private String key;

  private String errorMessage;

  private Set<String> dependsOn = new HashSet<>();

  public RowValidatorAdapter(String key, String errorMessage, Set<String> dependsOn) {
    this.sheetIndex = 1;
    this.key = key;
    this.errorMessage = errorMessage;
    this.dependsOn = dependsOn;
  }

  @Override
  public String getKey() {
    return key;
  }

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }

  @Override
  public int getSheetIndex() {
    return sheetIndex;
  }

  @Override
  public Set<String> getDependsOn() {
    return dependsOn;
  }

  @Override
  public boolean valid(Row row) {
    return customValidate(row);
  }

  /**
   * for customer access error message
   *
   * @param errorMessage error message
   */
  protected void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  protected abstract boolean customValidate(Row row);
}
