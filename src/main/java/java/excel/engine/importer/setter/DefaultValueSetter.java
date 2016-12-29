package java.excel.engine.importer.setter;

import java.excel.engine.util.FieldUtils;
import java.excel.engine.exception.ExcelProcessException;
import java.excel.engine.model.excel.Cell;
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
import java.util.List;


/**
 * default field value setter, using {@link BeanUtils#setProperty(Object, String, Object)}
 * <p>
 * Created by hanwen on 15-12-18.
 */
public class DefaultValueSetter {

  private static final Logger LOGGER = LoggerFactory.getLogger(DefaultValueSetter.class);

  static {
    ConvertUtils.register(new DateConverter(null), java.util.Date.class);
    ConvertUtils.register(new CalendarConverter(null), Calendar.class);
    ConvertUtils.register(new SqlDateConverter(null), java.sql.Date.class);
    ConvertUtils.register(new SqlTimeConverter(null), Time.class);
    ConvertUtils.register(new SqlTimestampConverter(null), Timestamp.class);
    ConvertUtils.register(new BigDecimalConverter(null), BigDecimal.class);
  }

  public void set(Object data, List<Cell> cells) {

    for (Cell cell : cells) {
      try {
        BeanUtils.setProperty(data, FieldUtils.detectRealField(cell.getField()), matches(data, cell.getField()) ? cell.getValue() : null);
      } catch (Exception e) {
        LOGGER.error(ExceptionUtils.getStackTrace(e));
        throw new ExcelProcessException(e);
      }
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
