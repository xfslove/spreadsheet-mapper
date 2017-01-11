package spreadsheet.mapper.w2o.validation.validator.row;


import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.meta.SheetMeta;

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * row values validator adapter, easy implements customer value validator extends this.
 * <p>
 * Created by hanwen on 4/26/16.
 */
public abstract class RowValidatorAdapter implements RowValidator {

  private String group;

  private String errorMessage;

  private Set<String> dependsOn = new LinkedHashSet<>();

  private Set<String> messageOnFields = new HashSet<>();

  public RowValidatorAdapter(String group, String errorMessage, String[] messageOnFields) {
    this(group, errorMessage, messageOnFields, null);
  }

  public RowValidatorAdapter(String group, String errorMessage, String[] messageOnFields, String[] dependsOn) {
    this.group = group;
    this.errorMessage = errorMessage;
    if (messageOnFields != null) {
      Collections.addAll(this.messageOnFields, messageOnFields);
    }
    if (dependsOn != null) {
      Collections.addAll(this.dependsOn, dependsOn);
    }
  }

  @Override
  public String getGroup() {
    return group;
  }

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }

  @Override
  public Set<String> getMessageOnFields() {
    return messageOnFields;
  }

  @Override
  public Set<String> getDependsOn() {
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
