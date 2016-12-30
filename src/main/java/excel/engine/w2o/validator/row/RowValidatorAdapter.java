package excel.engine.w2o.validator.row;


import excel.engine.model.core.Row;
import excel.engine.model.meta.SheetMeta;

import java.util.HashSet;
import java.util.Set;

/**
 * row values validator adapter, easy implements customer value validator extends this.
 * <p>
 * Created by hanwen on 4/26/16.
 */
public abstract class RowValidatorAdapter implements RowValidator {

  private String key;

  private String errorMessage;

  private Set<String> dependsOn = new HashSet<>();

  public RowValidatorAdapter(String key, String errorMessage, Set<String> dependsOn) {
    this.key = key;
    this.errorMessage = errorMessage;
    this.dependsOn = dependsOn;
  }

  @Override
  public String getGroup() {
    return key;
  }

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }

  @Override
  public Set<String> getDependsOnGroups() {
    return dependsOn;
  }

  @Override
  public boolean valid(Row row, SheetMeta sheetMeta) {
    return customValidate(row, sheetMeta);
  }

  /**
   * for customer access error message
   *
   * @param errorMessage error message
   */
  protected void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  protected abstract boolean customValidate(Row row, SheetMeta sheetMeta);
}
