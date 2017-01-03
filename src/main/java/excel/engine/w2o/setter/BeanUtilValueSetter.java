package excel.engine.w2o.setter;

import excel.engine.model.core.Cell;
import excel.engine.model.meta.FieldMeta;
import excel.engine.util.FieldUtils;
import excel.engine.w2o.processor.WorkbookProcessException;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.beanutils.ConvertUtils;
import org.apache.commons.beanutils.converters.*;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.Calendar;


/**
 * using {@link BeanUtils#setProperty(Object, String, Object)}, this use to fallback
 * <p>
 * Created by hanwen on 15-12-18.
 */
public class BeanUtilValueSetter implements ValueSetter {

  private static final Logger LOGGER = LoggerFactory.getLogger(BeanUtilValueSetter.class);

  {
    ConvertUtils.register(new DateConverter(null), java.util.Date.class);
    ConvertUtils.register(new CalendarConverter(null), Calendar.class);
    ConvertUtils.register(new SqlDateConverter(null), java.sql.Date.class);
    ConvertUtils.register(new SqlTimeConverter(null), Time.class);
    ConvertUtils.register(new SqlTimestampConverter(null), Timestamp.class);
    ConvertUtils.register(new BigDecimalConverter(null), BigDecimal.class);
  }

  @Override
  public void set(Object data, Cell cell, FieldMeta fieldMeta) {
    try {
      BeanUtils.setProperty(data, FieldUtils.detectRealField(fieldMeta.getName()), matches(data, fieldMeta.getName()) ? cell.getValue() : null);
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookProcessException(e);
    }
  }

  private boolean matches(Object data, String field) {
    Class fieldType = FieldUtils.getFieldType(data.getClass(), FieldUtils.detectRealField(field).split("\\."));

    return fieldType != null &&
        (Integer.class.equals(fieldType) || int.class.equals(fieldType) ||
            Long.class.equals(fieldType) || long.class.equals(fieldType) ||
            Double.class.equals(fieldType) || double.class.equals(fieldType) ||
            Float.class.equals(fieldType) || float.class.equals(fieldType) ||
            String.class.equals(fieldType));

  }
}
