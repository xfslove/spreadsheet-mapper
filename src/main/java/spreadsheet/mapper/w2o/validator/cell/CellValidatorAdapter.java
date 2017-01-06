package spreadsheet.mapper.w2o.validator.cell;

import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.core.Cell;
import org.apache.commons.lang3.StringUtils;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * <pre>
 * cell value validator adapter, easy implements customer value validator extends this.
 * extends this validator will skip custom valid when cell value is blank (default blank value means no need valid).
 * </pre>
 * Created by hanwen on 15-12-16.
 */
public abstract class CellValidatorAdapter implements CellValidator {

  private String group;

  private String matchField;

  private String errorMessage;

  private String messageOnField;

  private Set<String> dependsOn = new LinkedHashSet<>();

  public CellValidatorAdapter(String matchField, String errorMessage) {
    this.group = matchField;
    this.matchField = matchField;
    this.errorMessage = errorMessage;
    this.messageOnField = matchField;
  }

  public CellValidatorAdapter(String matchField, String errorMessage, String[] dependsOn) {
    this.group = matchField;
    this.matchField = matchField;
    this.errorMessage = errorMessage;
    this.messageOnField = matchField;
    if (dependsOn != null) {
      Collections.addAll(this.dependsOn, dependsOn);
    }
  }

  public CellValidatorAdapter(String group, String matchField, String errorMessage, String messageOnField, String[] dependsOn) {
    this.group = group;
    this.matchField = matchField;
    this.errorMessage = errorMessage;
    this.messageOnField = messageOnField;
    if (dependsOn != null) {
      Collections.addAll(this.dependsOn, dependsOn);
    }
  }

  @Override
  public String getGroup() {
    return group;
  }

  @Override
  public Set<String> getDependsOn() {
    return dependsOn;
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
  public String getMessageOnField() {
    return messageOnField;
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
