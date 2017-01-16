package spreadsheet.mapper.w2o.validation.validator.cell;

import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.w2o.validation.WorkbookValidateException;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * <pre>
 * cell value validator adapter, easy implements customer cell validator extends this.
 * extends this will skip custom valid when cell value is blank (default blank value means no need valid).
 * </pre>
 * <p>
 * Created by hanwen on 2017/1/11.
 */
public abstract class CellValidatorAdapter<V extends CellValidatorAdapter<V>> implements CellValidator {

  private String group;

  private Set<String> dependsOn = new LinkedHashSet<>();

  private String matchField;

  private String errorMessage;

  private String messageOnField;

  /**
   * @param matchField {@link CellValidator#getMatchField()}
   * @return {@link V}
   */
  public V matchField(String matchField) {
    this.matchField = matchField;
    return getThis();
  }

  /**
   * @param errorMessage {@link CellValidator#getErrorMessage()}
   * @return {@link V}
   */
  public V errorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
    return getThis();
  }

  /**
   * if empty default is {@link #getMatchField()}
   *
   * @param messageOnField {@link CellValidator#getMessageOnField()}
   * @return {@link V}
   */
  public V messageOnField(String messageOnField) {
    this.messageOnField = messageOnField;
    return getThis();
  }

  /**
   * @param dependsOn {@link CellValidator#getDependsOn()}
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
   * if empty default is {@link #getMatchField()}
   *
   * @param group {@link CellValidator#getGroup()}
   * @return {@link V}
   */
  public V group(String group) {
    this.group = group;
    return getThis();
  }

  @Override
  public boolean valid(Cell cell, FieldMeta fieldMeta) {
    return StringUtils.isBlank(cell.getValue()) || customValid(cell, fieldMeta);
  }

  @Override
  public String getGroup() {
    if (StringUtils.isBlank(group)) {
      return getMatchField();
    }
    return group;
  }

  @Override
  public String getMatchField() {
    if (StringUtils.isBlank(matchField)) {
      throw new WorkbookValidateException("cell validator match field must be set");
    }
    return matchField;
  }

  @Override
  public Set<String> getDependsOn() {
    return dependsOn;
  }

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }

  @Override
  public String getMessageOnField() {
    if (StringUtils.isBlank(messageOnField)) {
      return getMatchField();
    }
    return messageOnField;
  }

  protected abstract V getThis();

  protected abstract boolean customValid(Cell cell, FieldMeta fieldMeta);
}
