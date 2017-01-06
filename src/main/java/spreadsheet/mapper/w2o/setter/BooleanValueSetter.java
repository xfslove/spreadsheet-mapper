package spreadsheet.mapper.w2o.setter;

import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.w2o.processor.WorkbookProcessException;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.utils.FieldUtils;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * boolean field value setter
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class BooleanValueSetter<T> extends FieldValueSetterAdapter<T> {

  private static final Logger LOGGER = LoggerFactory.getLogger(BooleanValueSetter.class);

  private Set<String> supportedTrueStrings = new HashSet<>();

  private Set<String> supportedFalseStrings = new HashSet<>();

  public BooleanValueSetter(String[] supportedTrueStrings, String[] supportedFalseStrings, String matchField) {
    super(matchField);
    if (supportedTrueStrings != null) {
      Collections.addAll(this.supportedTrueStrings, supportedTrueStrings);
    }
    if (supportedFalseStrings != null) {
      Collections.addAll(this.supportedFalseStrings, supportedFalseStrings);
    }
  }

  @Override
  public void set(T object, Cell cell, FieldMeta fieldMeta) {
    try {
      String stringValue = cell.getValue();
      Boolean booleanValue = null;

      if (supportedTrueStrings.contains(stringValue)) {
        booleanValue = Boolean.TRUE;
      } else if (supportedFalseStrings.contains(stringValue)) {
        booleanValue = Boolean.FALSE;
      }

      BeanUtils.setProperty(object, FieldUtils.detectRealField(fieldMeta.getName()), booleanValue);
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookProcessException(e);
    }
  }
}
