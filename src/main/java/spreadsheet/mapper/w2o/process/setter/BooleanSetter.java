package spreadsheet.mapper.w2o.process.setter;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.utils.FieldUtils;
import spreadsheet.mapper.w2o.process.WorkbookProcessException;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * boolean field value setter
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class BooleanSetter<T> extends FieldSetterAdapter<T, BooleanSetter<T>> {

  private static final Logger LOGGER = LoggerFactory.getLogger(BooleanSetter.class);

  private Set<String> trueStrings = new HashSet<>();

  private Set<String> falseStrings = new HashSet<>();

  public BooleanSetter<T> toTrue(String... trueStrings) {
    if (trueStrings == null) {
      return getThis();
    }
    Collections.addAll(this.trueStrings, trueStrings);
    return getThis();
  }

  public BooleanSetter<T> toFalse(String... falseStrings) {
    if (falseStrings == null) {
      return getThis();
    }
    Collections.addAll(this.falseStrings, falseStrings);
    return getThis();
  }

  @Override
  protected BooleanSetter<T> getThis() {
    return this;
  }

  @Override
  public void customSet(T object, Cell cell, FieldMeta fieldMeta) {
    try {
      String stringValue = cell.getValue();
      Boolean booleanValue = null;

      if (trueStrings.contains(stringValue)) {
        booleanValue = Boolean.TRUE;
      } else if (falseStrings.contains(stringValue)) {
        booleanValue = Boolean.FALSE;
      }

      BeanUtils.setProperty(object, FieldUtils.detectRealFieldName(fieldMeta), booleanValue);
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookProcessException(e);
    }
  }
}
