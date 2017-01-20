package spreadsheet.mapper.w2o.validation.validator.row;


import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.meta.SheetMeta;

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * row values validator adapter, easy implements customer row validator extends this.
 * <p>
 * Created by hanwen on 4/26/16.
 */
public abstract class RowValidatorAdapter<V extends RowValidatorAdapter<V>> implements RowValidator {

  private String group;

  private Set<String> dependsOn = new LinkedHashSet<>();

  private String errorMessage;

  private Set<String> messageOnFields = new HashSet<>();

  /**
   * @param errorMessage {@link RowValidator#getErrorMessage()}
   * @return {@link V}
   */
  public V errorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
    return getThis();
  }

  /**
   * @param messageOnFields {@link RowValidator#getMessageOnFields()}
   * @return {@link V}
   */
  public V messageOnFields(String... messageOnFields) {
    if (messageOnFields == null) {
      return getThis();
    }
    Collections.addAll(this.messageOnFields, messageOnFields);
    return getThis();
  }

  /**
   * @param dependsOn {@link RowValidator#getDependsOn()}
   * @return {@link V}
   */
  public V dependsOn(String... dependsOn) {
    if (dependsOn == null) {
      return getThis();
    }
    Collections.addAll(this.dependsOn, dependsOn);
    return getThis();
  }

  /**
   * @param group {@link RowValidator#getGroup()}
   * @return {@link V}
   */
  public V group(String group) {
    this.group = group;
    return getThis();
  }

  @Override
  public boolean valid(Row row, SheetMeta sheetMeta) {
    return customValid(row, sheetMeta);
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
  public Set<String> getMessageOnFields() {
    return messageOnFields;
  }

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }

  protected abstract V getThis();

  protected abstract boolean customValid(Row row, SheetMeta sheetMeta);
}
