package excel.engine.w2o.setter;

import excel.engine.model.core.Cell;
import excel.engine.model.meta.FieldMeta;
import excel.engine.util.BooleanUtils;
import excel.engine.util.FieldUtils;
import excel.engine.w2o.processor.ExcelProcessException;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * boolean field value setter
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class BooleanValueSetter extends FieldValueSetterAdapter {

  private static final Logger LOGGER = LoggerFactory.getLogger(BooleanValueSetter.class);

  public BooleanValueSetter(String matchField) {
    super(matchField);
  }

  @Override
  public void set(Object data, Cell cell, FieldMeta fieldMeta) {
    try {
      BeanUtils.setProperty(data, FieldUtils.detectRealField(fieldMeta.getName()), BooleanUtils.stringToBoolean(cell.getValue()));
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelProcessException(e);
    }
  }
}
