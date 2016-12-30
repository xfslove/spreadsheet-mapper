package excel.engine.w2o.validator.cell;

import excel.engine.model.core.Cell;
import excel.engine.model.meta.FieldMeta;
import org.apache.commons.lang3.StringUtils;

import java.util.HashSet;
import java.util.Set;

/**
 * <pre>
 * cell value validator adapter, easy implements customer value validator extends this.
 * extends this validator will skip valid when cell value is blank.
 * </pre>
 * Created by hanwen on 15-12-16.
 */
public abstract class CellValidatorAdapter implements CellValidator {

  private String key;

  private String matchField;

  private String errorMessage;

  private Set<String> dependsOn = new HashSet<>();

  public CellValidatorAdapter(String matchField, String errorMessage) {
    this.key = matchField;
    this.matchField = matchField;
    this.errorMessage = errorMessage;
  }

  public CellValidatorAdapter(String key, String matchField, String errorMessage) {
    this.key = key;
    this.matchField = matchField;
    this.errorMessage = errorMessage;
  }

  public CellValidatorAdapter(String matchField, String errorMessage, Set<String> dependsOn) {
    this.key = matchField;
    this.matchField = matchField;
    this.errorMessage = errorMessage;
    this.dependsOn = dependsOn;
  }

  public CellValidatorAdapter(String key, String matchField, String errorMessage, Set<String> dependsOn) {
    this.key = key;
    this.matchField = matchField;
    this.errorMessage = errorMessage;
    this.dependsOn = dependsOn;
  }

  @Override
  public String getGroup() {
    return key;
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
  public Set<String> getDependsOnGroups() {
    return dependsOn;
  }

  @Override
  public boolean valid(Cell cell, FieldMeta fieldMeta) {
    return StringUtils.isBlank(cell.getValue()) || customValidate(cell, fieldMeta);
  }

  /**
   * for customer access error message
   *
   * @param errorMessage error message
   */
  protected void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  protected abstract boolean customValidate(Cell cell, FieldMeta fieldMeta);

}
